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
import           Network.AWS.ECR
import qualified Network.AWS.STS as STS
import           OktaClient
import           STS
import           Text.XML
import           Text.XML.Lens
import           Types



data SamlAccountSession =
  SamlAccountSession { sasAwsProfile :: !AWSProfile
                     , sasAccountID :: !OktaAWSAccountID
                     , sasChosenSamlRole :: !(Maybe SamlRole)
                     , sasCredentials :: !STS.Credentials
                     , sasDockerAuths :: ![AuthorizationData]
                     } deriving (Eq, Show)

data SamlOrgSession =
  SamlOrgSession { sosOktaOrg :: !OktaOrg
                 , sosNeedsMFA :: !Bool
                 , sosAccountSessions :: !(NonEmpty SamlAccountSession)
                 }


main:: IO ()
main = runWithArgs $ runApp $ do
  cr <- getUserCredentials

  -- First login, possibly ask user some questions
  initialOrgSessions <- createInitialOrgSessions >>= mapM (refreshOrgSession cr)

  whenM keepReloading $ foldM_ (\s act -> act s) initialOrgSessions
                          (NL.repeat (refreshAllOrgSessions cr))



refreshAllOrgSessions :: UserCredentials
                      -> NonEmpty SamlOrgSession
                      -> App (NonEmpty SamlOrgSession)
refreshAllOrgSessions cr sess = do
  $(logInfo) "Refreshed AWS session."

  liftIO $ threadDelay (59 * 60 * 1000000) -- 59min, temporary token has 1h TTL

  $(logInfo) "Refreshing AWS session ..."

  doRefresh <- if null $ NL.filter sosNeedsMFA sess
               then ("y" ==) <$> askUser True "Refresh session? (y/n) >" -- if we get session from Okta first it may expire by the time user pays any attention to this, need to ask first
               else return True -- probably on a trusted network, just try to refresh

  unless doRefresh $ error "Session refresh cancelled, exiting ..."

  updatedSessions <- mapM (refreshOrgSession cr) sess

  let allUpdatedAccountSessions = concat (NL.toList . sosAccountSessions <$> NL.toList updatedSessions)
      allUpdatedAwsCreds = (\SamlAccountSession{..} -> (sasAwsProfile, sasCredentials)) <$> allUpdatedAccountSessions
      allUpdatedDockerAuths = concat (sasDockerAuths <$> allUpdatedAccountSessions)

  $(logDebug) $ T.pack $ "Updating AWS creds to " <> show allUpdatedAwsCreds
  updateAwsCreds allUpdatedAwsCreds

  $(logDebug) $ T.pack $ "Updating Docker auths to " <> show allUpdatedDockerAuths
  updateDockerConfig allUpdatedDockerAuths

  return updatedSessions


refreshOrgSession :: UserCredentials
                  -> SamlOrgSession
                  -> App SamlOrgSession
refreshOrgSession cr sos@SamlOrgSession{..} = do
  errorOrRes <- oktaAuthenticate sosOktaOrg $ AuthRequestUserCredentials cr

  res <- case errorOrRes
           of Left e -> error $ "Unexpected Okta response: " <> show e <> " please check your credentials!"
              Right r -> return r

  -- May need to try MFA
  (mfaRequired, sessionTok) <- case res
                                 of AuthResponseSuccess st -> return (False, st)
                                    AuthResponseMFARequired st mfas -> (\t -> (True, t)) <$> askForMFA sosOktaOrg st mfas
                                    AuthResponseOther e -> error $ "Unexpected Okta response: " <> show e

  updatedAccountSessions <- mapM (refreshAccountSession sosOktaOrg sessionTok) sosAccountSessions
  return $ sos { sosAccountSessions = updatedAccountSessions
               , sosNeedsMFA = mfaRequired
               }


refreshAccountSession :: OktaOrg
                      -> SessionToken
                      -> SamlAccountSession
                      -> App SamlAccountSession
refreshAccountSession oOrg sTok sas@SamlAccountSession{..} = do
  saml <- getOktaAWSSaml oOrg sasAccountID sTok
  samlRole <- case sasChosenSamlRole
                of Just sr -> return sr
                   Nothing -> chooseSamlRole (parseSamlAssertion saml)

  (awsCreds, dockerAuths) <- awsAssumeRole saml samlRole

  return $ sas { sasChosenSamlRole = Just samlRole
               , sasCredentials = awsCreds
               , sasDockerAuths = dockerAuths }


askForMFA :: OktaOrg
          -> StateToken
          -> [MFAFactor]
          -> App SessionToken
askForMFA oOrg st mfas = do
  let totpMfas = findTotpFactors mfas

  when (null totpMfas) $ error "Sorry, an MFA auth is required to continue and no TOTP factors were available. Please configure a TOTP device (e.g. a mobile Okta app) or re-try via VPN connection."

  MFAFactor{..} <- chooseOne $ NL.fromList $
                     fmap (\(nc, f@MFAFactor{..}) -> nc mfaProvider f)
                     (zip numericChoices totpMfas)

  pc <- MFAPassCode <$> askUser True ("Please enter " <> unOktaOrg oOrg <> " " <> mfaProvider <> " MFA code> ")

  errorOrRes <- oktaMFAVerify oOrg $ AuthRequestMFATOTPVerify st mfaId pc

  case errorOrRes
    of Left e -> do _ <- error $ "Unexpected Okta response: " <> show e <> " please try again."
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
createInitialOrgSessions :: App (NonEmpty SamlOrgSession)
createInitialOrgSessions = do
  now <- liftIO getCurrentTime

  samlConfs <- getOktaSamlConfig
  $(logInfo) $ "Using AWS profiles " <> tshow ((unAwsProfile . ocAwsProfile) <$> samlConfs)

  let samlConfsByOrg = NL.groupBy (\sc1 sc2 -> ocOrg sc1 == ocOrg sc2) samlConfs

      emptyCreds = STS.credentials "" "" "" now

      initialAccountSession OktaSamlConfig{..} =
        SamlAccountSession ocAwsProfile ocOktaAwsAccountId Nothing emptyCreds []

      initialOrgSession OktaSamlConfig{..} =
        SamlOrgSession ocOrg False

      initialOrgSessions = (\scs -> initialOrgSession (NL.head scs) (initialAccountSession <$> scs)) <$> samlConfsByOrg

  return $ NL.fromList initialOrgSessions
