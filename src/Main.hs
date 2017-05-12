{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where


import           AWSCredsFile
import           App
import           Args
import           Control.Bool
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import           DockerConfig
import qualified Network.AWS.STS as STS
import           OktaClient
import           STS
import           Text.XML
import           Text.XML.Lens
import           Types



main:: IO ()
main = runWithArgs $ runApp $ do
  cr <- getUserCredentials
  _ <- createInitialOrgSessions >>= setSamlSession

  -- First login, possibly ask user some questions
  _ <- updateSamlSession (refreshSamlSession cr)

  void . whenM keepReloading $ do
    liftIO $ threadDelay (59 * 60 * 1000000) -- 59min, temporary token has 1h TTL

    uMfa <- usedMFA

    doRefresh <- if uMfa
                then ("y" ==) <$> askUser True "Refresh session? (y/n) >" -- if we get session from Okta first it may expire by the time user pays any attention to this, need to ask first
                else return True -- probably on a trusted network, just try to refresh

    unless doRefresh $ error "Session refresh cancelled, exiting ..."
    useMFA False -- reset state, network may have switched by now
    updateSamlSession (refreshSamlSession cr)



refreshSamlSession :: UserCredentials
                   -> SamlSession
                   -> App SamlSession
refreshSamlSession cr sess = do
  $(logInfo) "Refreshing AWS session ..."

  allUpdatedAccountSessions <- traverse (refreshAccountSession cr) sess

  let allUpdatedAwsCreds = (\SamlAccountSession{..} -> (sasAwsProfile, sasCredentials)) <$> allUpdatedAccountSessions
      allUpdatedDockerAuths = concat (sasDockerAuths <$> allUpdatedAccountSessions)

  $(logDebug) $ T.pack $ "Updating AWS creds to " <> show allUpdatedAwsCreds
  updateAwsCreds allUpdatedAwsCreds

  $(logDebug) $ T.pack $ "Updating Docker auths to " <> show allUpdatedDockerAuths
  updateDockerConfig allUpdatedDockerAuths

  $(logInfo) "Refreshed AWS session."
  return allUpdatedAccountSessions



refreshAccountSession :: UserCredentials
                      -> SamlAccountSession
                      -> App SamlAccountSession
refreshAccountSession uc sas@SamlAccountSession{..} = do

  errorOrRes <- oktaAuthenticate sasOktaOrg $ AuthRequestUserCredentials uc

  res <- case errorOrRes
           of Left e -> error $ "Unexpected Okta response: " <> show e <> " please check your credentials!"
              Right r -> return r

  -- May need to try MFA
  sessionTok <- case res
                  of AuthResponseSuccess st -> return st
                     AuthResponseMFARequired st mfas -> askForMFA sasOktaOrg st mfas
                     AuthResponseOther e -> error $ "Unexpected Okta response: " <> show e

  saml <- getOktaAWSSaml sasOktaOrg sasAccountID sessionTok
  samlRole <- case sasChosenSamlRole
                of Just sr -> return sr
                   Nothing -> chooseSamlRole (parseSamlAssertion saml)

  (awsCreds, dockerAuths) <- awsAssumeRole saml samlRole

  let updatedSession = sas { sasChosenSamlRole = Just samlRole
                           , sasCredentials = awsCreds
                           , sasDockerAuths = dockerAuths }

  return updatedSession



askForMFA :: OktaOrg
          -> StateToken
          -> [MFAFactor]
          -> App SessionToken
askForMFA oOrg st mfas = do
  -- Mark that we needed MFA in this session. When we keep reloading we need to wait until the user
  -- is ready until acquiring state token. Otherwise it may expire by the time user reacts.
  useMFA True

  let totpMfas = findTotpFactors mfas

  when (null totpMfas) $ error "Sorry, an MFA auth is required to continue and no TOTP factors were available. Please configure a TOTP device (e.g. a mobile Okta app) or re-try via VPN connection."

  (fid, pc) <- do
    MFAFactor{..} <- chooseOne $ NL.fromList $
                        fmap (\(nc, f@MFAFactor{..}) -> nc mfaProvider f)
                        (zip numericChoices totpMfas)
    pc <- MFAPassCode <$> askUser True ("Please enter " <> unOktaOrg oOrg <> " " <> mfaProvider <> " MFA code> ")
    return (mfaId, pc)


  errorOrRes <- oktaMFAVerify oOrg $ AuthRequestMFATOTPVerify st fid pc

  case errorOrRes
    of Left e -> do _ <- $(logError) $ "Unexpected Okta response: " <> tshow e <> " please try again."
                    askForMFA oOrg st mfas
       Right r -> case r
                    of AuthResponseSuccess s -> return s
                       e                     -> error $ "Unexpected Okta response: " <> show e



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

      mkRole [pArn, rArn] = SamlRole rArn pArn
      mkRole x = error $ "Couldn't parse SAML role ARNs from " <> show x

      extractRoles = mkRole <$> fmap (T.splitOn ",") roles

   in fromMaybe (error $ "Sorry, couldn't extract any role ARNs from " <> show doc) (NL.nonEmpty extractRoles)



-- | Init session data with some defaults and dummy values
createInitialOrgSessions :: App [SamlAccountSession]
createInitialOrgSessions = do
  now <- liftIO getCurrentTime

  samlConfs <- getOktaSamlConfig
  $(logInfo) $ "Using AWS profiles " <> tshow ((unAwsProfile . ocAwsProfile) <$> samlConfs)

  let emptyCreds = STS.credentials "" "" "" now

      initialAccountSession OktaSamlConfig{..} =
        SamlAccountSession ocOrg ocAwsProfile ocOktaAwsAccountId Nothing emptyCreds []

      initialAccountSessions = initialAccountSession <$> samlConfs

  return $ NL.toList initialAccountSessions
