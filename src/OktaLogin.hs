{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module OktaLogin (
  oktaLogin
) where


import           App
import           AWSCredsFile
import           Control.Concurrent
import           Control.Lens ((^..))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Loops
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NL
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           DockerLogin
import           OktaClient
import           STS
import           Text.XML
import           Text.XML.Lens
import           Types


-- | Login to AWS with Okta
--   Implementation of the main top level command (login to AWS through Okta).
oktaLogin :: App ()
oktaLogin = do
  startedWithArgs <- getArgs
  $(logDebugSH) startedWithArgs

  cr <- getUserCredentials
  mte <- mfaFactorToEnroll
  _ <- createInitialOrgSessions >>= setSamlSession

  -- First login, possibly ask user some questions
  _ <- updateSamlSession (refreshSamlSession cr mte)

  minSessionDurationSeconds <- (\configs -> minimum (ocSessionDurationSeconds <$> configs)) <$> getOktaAWSConfig
  let sleepTime = minSessionDurationSeconds - 1 -- 1 sec before expiration

  whileM_ keepReloading $ do
    $(logDebug) $ "Sleeping for " <> tshow sleepTime <> " seconds ..."
    liftIO $ threadDelay $ fromIntegral sleepTime * 1000000

    uMfa <- usedMFA

    doRefresh <- if uMfa
                then ("y" ==) <$> askUser True "Refresh session? (y/n) >" -- if we get session from Okta first it may expire by the time user pays any attention to this, need to ask first
                else return True -- probably on a trusted network, just try to refresh

    unless doRefresh $ error "Session refresh cancelled, exiting ..."
    useMFA False -- reset state, network may have switched by now
    updateSamlSession (refreshSamlSession cr mte)


-- | Refresh AWS (and possibly docker) auth sessions for all profiles listed on the command line
refreshSamlSession :: UserCredentials
                   -> MFAFactorType
                   -> SamlSession
                   -> App SamlSession
refreshSamlSession cr mte sess = do
  $(logInfo) "Refreshing AWS session ..."

  allUpdatedAccountSessions <- traverse (refreshAccountSession cr mte) sess

  let allUpdatedAwsCreds = (\SamlAccountSession{..} -> (sasAwsProfile, sasAwsCredentials)) <$> allUpdatedAccountSessions
      allUpdatedDockerAuths = concat (sasDockerAuths <$> allUpdatedAccountSessions)

  $(logDebug) $ T.pack $ "Updating AWS creds to " <> show allUpdatedAwsCreds
  updateAwsCreds allUpdatedAwsCreds

  unless (null allUpdatedDockerAuths) $ do
    $(logDebug) $ T.pack $ "Updating Docker auths to " <> show allUpdatedDockerAuths
    dockerLogin allUpdatedDockerAuths

  $(logInfo) "Refreshed AWS session."
  return allUpdatedAccountSessions

-- | Refresh AWS and possibly docker auths for a single AWS profile (usually associated with an account)
refreshAccountSession :: UserCredentials
                      -> MFAFactorType
                      -> SamlAccountSession
                      -> App SamlAccountSession
refreshAccountSession uc mte sas@SamlAccountSession{..} = do

  oktaOrg <- parseOktaOrg sasEmbedLink
  errorOrRes <- oktaAuthenticate oktaOrg $ AuthRequestUserCredentials uc

  res <- case errorOrRes
           of Left e -> error $ "Unexpected Okta response: " <> show e <> " please check your credentials!"
              Right r -> return r

  -- May need to try MFA
  sessionTok <- case res
                  of AuthResponseSuccess st            -> return st
                     AuthResponseMFAChallenge st _ fid -> poll oktaOrg st fid
                     AuthResponseMFARequired st mfas   -> case mte
                       of MFAFactorTOTP -> askForMFATOTP oktaOrg st mfas
                          MFAFactorPush -> mfaPush oktaOrg st mfas
                     AuthResponseOther e -> error $ "Unexpected Okta response: " <> show e

  saml <- getOktaAWSSaml sasEmbedLink sessionTok
  samlRole <- case sasChosenSamlRole
                of Just sr -> return sr
                   Nothing -> chooseSamlRole (parseSamlAssertion saml)

  ecrLogin <- doECRLogin sas

  (awsCreds, dockerAuths) <- awsAssumeRole ecrLogin sasSessionDurationSeconds  saml samlRole

  let updatedSession = sas { sasChosenSamlRole = Just samlRole
                           , sasAwsCredentials = awsCreds
                           , sasDockerAuths = dockerAuths }

  return updatedSession

poll :: OktaOrg -> StateToken -> MFAFactorID -> App SessionToken
poll oOrg st fid = do

  errorOrRes <- oktaMFAVerify oOrg $ AuthRequestMFAPollVerify st fid

  case errorOrRes
    of Left e -> error $ "Unexpected Okta response: " <> show e
       Right r -> case r
                    of AuthResponseSuccess s -> return s
                       AuthResponseMFAChallenge st' cr fid' ->
                         if cr == MFAFactorWaiting
                         then poll oOrg st' fid'
                         else error $ "Unexpected Okta MFA challenge result: " <> show cr
                       e                     -> error $ "Unexpected Okta response: " <> show e

mfaPush :: OktaOrg
          -> StateToken
          -> [MFAFactor]
          -> App SessionToken
mfaPush oOrg st mfas = do
  let pushMfas = findPushFactors mfas

  fid <- do
    MFAFactor{..} <- chooseOne $ NL.fromList $
                        fmap (\(nc, f@MFAFactor{..}) -> nc mfaProvider f)
                        (zip numericChoices pushMfas)
    return mfaId

  errorOrRes <- oktaMFAVerify oOrg $ AuthRequestMFAPushVerify st fid (RememberDevice True) (AutoPush True)

  case errorOrRes
    of Left e -> error $ "Unexpected Okta response: " <> show e
       Right r -> case r
                    of AuthResponseSuccess s               -> return s
                       AuthResponseMFAChallenge st' _ fid' -> poll oOrg st' fid'
                       e                                   -> error $ "Unexpected Okta response: " <> show e

-- | Ask user to enter 2nd factor token if required
askForMFATOTP :: OktaOrg
          -> StateToken
          -> [MFAFactor]
          -> App SessionToken
askForMFATOTP oOrg st mfas = do
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
                    askForMFATOTP oOrg st mfas
       Right r -> case r
                    of AuthResponseSuccess s -> return s
                       e                     -> error $ "Unexpected Okta response: " <> show e


-- | Ask user to choose SAML role if more then one is available
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
      roles = doc ^.. root . named "Response" ...
                             named "Assertion" ...
                             named "AttributeStatement" ...
                             (named "Attribute" . attributeIs "Name" "https://aws.amazon.com/SAML/Attributes/Role") ...
                             (named "AttributeValue" . text)

      mkRole [pArn, rArn] = SamlRole rArn pArn
      mkRole x = error $ "Couldn't parse SAML role ARNs from " <> show x

      extractRoles = mkRole <$> fmap (T.splitOn ",") roles

   in fromMaybe (error $ "Sorry, couldn't extract any role ARNs from " <> show doc) (NL.nonEmpty extractRoles)


-- | Init session data with some defaults and dummy values
createInitialOrgSessions :: App [SamlAccountSession]
createInitialOrgSessions = do
  samlConfs <- getOktaAWSConfig
  $(logInfo) $ "Using AWS profiles " <> tshow (unAwsProfile . ocAwsProfile <$> samlConfs)

  let emptyCreds = SamlAWSCredentials "" "" ""

      initialAccountSession OktaAWSConfig{..} =
        SamlAccountSession ocEmbedLink (fromMaybe False ocECRLogin) ocSessionDurationSeconds ocAwsProfile Nothing emptyCreds []

      initialAccountSessions = initialAccountSession <$> samlConfs

  return $ NL.toList initialAccountSessions
