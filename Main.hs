{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where


import           AWSCredsFile
import           App
import           Args
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Loops
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           DockerConfig
import           OktaClient
import           STS
import           Text.XML
import           Text.XML.Lens
import           Types


main:: IO ()
main = runWithArgs $ runApp $ do
  cr <- getUserCredentials

  (needsMfa, samlRole) <- refreshSession cr Nothing

  _ <- whileM keepReloading $ do
    $(logInfo) "Refreshed AWS session."

    liftIO $ threadDelay (59 * 60 * 1000000) -- 59min, temporary token has 1h TTL

    $(logInfo) "Refreshing AWS session ..."

    doRefresh <- if needsMfa
                 then fmap ((==) "y") $ askUser True "Refresh session? (y/n) >" -- if we get session from Okta first it may expire by the time user pays any attention to this, need to ask first
                 else return True -- probably on a trusted network, just try to refresh
    if doRefresh
    then fmap (const ()) $ refreshSession cr (Just samlRole) -- keep credentials and a choice of SAML role
    else return ()

  return ()



refreshSession :: UserCredentials
               -> Maybe SamlRole -- ^ user's choice of SamlRole
               -> App (Bool, SamlRole) -- ^ whether or not MFA was needed and the choice of SAML role
refreshSession cr maybeSr = do
  errorOrRes <- oktaAuthenticate $ AuthRequestUserCredentials cr

  res <- case errorOrRes
           of Left e -> error $ "Unexpected Okta response: " <> (show e) <> " please check your credentials!"
              Right r -> return r

  -- May need to try MFA
  (mfaRequired, sessionTok) <- case res
                                 of AuthResponseSuccess st -> return (False, st)
                                    AuthResponseMFARequired st mfas -> fmap (\t -> (True, t)) $ askForMFA st mfas
                                    AuthResponseOther e -> error $ "Unexpected Okta response: " <> (show e)

  saml <- getOktaAWSSaml sessionTok
  samlRole <- case maybeSr
                of Just sr -> return sr
                   Nothing -> chooseSamlRole (parseSamlAssertion saml)

  (awsCreds, dockerAuths) <- awsAssumeRole saml samlRole

  $(logDebug) $ T.pack $ "Updating AWS creds from " <> (show awsCreds)
  updateAwsCreds awsCreds

  $(logDebug) $ T.pack $ "Updating Docker auths from " <> (show dockerAuths)
  updateDockerConfig dockerAuths

  return (mfaRequired, samlRole)


askForMFA :: StateToken
          -> [MFAFactor]
          -> App SessionToken
askForMFA st mfas = do
  let totpMfas = findTotpFactors mfas

  when (null totpMfas) $ error "Sorry, an MFA auth is required to continue and no TOTP factors were available. Please configure a TOTP device (e.g. a mobile Okta app) or re-try via VPN connection."

  MFAFactor{..} <- chooseOne $ NL.fromList $
                     fmap (\(nc, f@MFAFactor{..}) -> nc mfaProvider f)
                     (zip numericChoices totpMfas)

  pc <- MFAPassCode <$> askUser True ("Please enter " <> mfaProvider <> " MFA code> ")

  errorOrRes <- oktaMFAVerify $ AuthRequestMFATOTPVerify st mfaId pc

  case errorOrRes
    of Left e -> do _ <- error $ "Unexpected Okta response: " <> (show e) <> " please try again."
                    askForMFA st mfas
       Right r -> case r
                    of AuthResponseSuccess s -> return s
                       e                     -> error $ "Unexpected Okta response: " <> (show e)


chooseSamlRole :: NonEmpty SamlRole
               -> App SamlRole
chooseSamlRole srs =
  chooseOne $ fmap (\(nc, sr@SamlRole{..}) -> nc srRoleARN sr) (NL.zip (NL.fromList numericChoices) srs)



-- | Okta gives us a Base64 encoded XML doc with assertions, decode available roles.
-- https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language
parseSamlAssertion :: SamlAssertion
                   -> NonEmpty SamlRole
parseSamlAssertion (SamlAssertion sa) =
  let doc = parseLBS_ def ((LB.fromStrict . B64.decodeLenient . TE.encodeUtf8) sa)
      roles = doc ^.. root . named "Response" ./
                             named "Assertion" ./
                             named "AttributeStatement" ./
                             (named "Attribute" . attributeIs "Name" "https://aws.amazon.com/SAML/Attributes/Role") ./
                             (named "AttributeValue" . text)

      mkRole (pArn : rArn : []) = SamlRole rArn pArn
      mkRole x = error $ "Couldn't parse SAML role ARNs from " <> (show x)

      extractRoles = fmap mkRole $ fmap (T.splitOn ",") roles

   in case NL.nonEmpty extractRoles
        of Nothing -> error $ "Sorry, couldn't extract any role ARNs from " <> (show doc)
           Just x  -> x
