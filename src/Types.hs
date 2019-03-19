{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}


-- | Type definitions
module Types (
  AWSProfile(..)
, AppConfig(..)
, AuthRequestMFAVerify(..)
, AuthRequestUserCredentials(..)
, InteractiveChoce(..)
, MFAFactor(..)
, MFAFactorType(..)
, MFAFactorResult(..)
, MFAFactorID(..)
, MFAPassCode(..)
, OktaAWSConfig(..)
, OktaAuthResponse(..)
, OktaEmbedLink(..)
, OktaError(..)
, OktaOrg(..)
, Password(..)
, RememberDevice(..)
, AutoPush(..)
, RequestPath
, SamlAWSCredentials(..)
, SamlAccountSession(..)
, SamlAssertion(..)
, SamlRole(..)
, SamlSession
, SessionToken(..)
, StateToken(..)
, UserCredentials
, UserName(..)
) where


import           Control.Lens ((^..), (^?!))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.List.NonEmpty
import           Data.String (IsString)
import           Data.Text (Text, unpack)
import qualified Network.AWS.Data.ByteString as AWSBS
import qualified Network.AWS.ECR as ECR
import           Network.AWS.Prelude (Natural)
import qualified Network.AWS.Types as AWST
import           Network.AWS.Data.Text --(FromText)


newtype UserName = UserName { unUserName :: Text } deriving (Eq, Show)
newtype Password = Password { unPassword :: Text } deriving (Eq, Show)

type UserCredentials = (UserName, Password)

newtype MFAFactorID = MFAFactorID { unMfaFactorId :: Text } deriving (Eq, Show, Ord, FromJSON, ToJSON)
newtype MFAPassCode = MFAPassCode { unMfaPassCode :: Text } deriving (Eq, Show, Ord, FromJSON, ToJSON)

newtype AutoPush = AutoPush { unAutoPush :: Bool } deriving (Eq, Show, Ord, FromJSON, ToJSON)
newtype RememberDevice = RememberDevice { unRememberDevice :: Bool } deriving (Eq, Show, Ord, FromJSON, ToJSON)


newtype OktaOrg =
  OktaOrg { unOktaOrg :: Text } deriving (Eq, Show, FromJSON, ToJSON, IsString)

newtype AWSProfile =
  AWSProfile { unAwsProfile :: Text } deriving (Eq, Show, FromJSON, ToJSON, IsString)

newtype OktaEmbedLink =
  OktaEmbedLink { unOktaEmbedLink :: Text } deriving (Eq, Show, FromJSON, ToJSON, IsString)

data OktaAWSConfig =
  OktaAWSConfig { ocEmbedLink :: !OktaEmbedLink
                , ocAwsProfile :: !AWSProfile
                , ocDefault :: !(Maybe Bool)
                , ocECRLogin :: !(Maybe Bool)
                , ocSessionDurationSeconds :: !Natural
                } deriving (Eq, Show)

$(deriveJSON (aesonPrefix snakeCase){ omitNothingFields = True } ''OktaAWSConfig)

newtype AppConfig = AppConfig { unAppConfig:: NonEmpty OktaAWSConfig }

$(deriveJSON (aesonPrefix snakeCase) ''AppConfig)



-- | For readability.
type RequestPath = Text

-- | Result of a successful login.
newtype SessionToken = SessionToken { unSessionToken :: Text } deriving (Eq, Show, FromJSON, ToJSON)

-- | Token used to chain calls when initial login was not successful.
newtype StateToken = StateToken { unStateToken :: Text } deriving (Eq, Show, FromJSON, ToJSON)

-- | AWS secrets we get from a SAML assertion
data SamlAWSCredentials =
  SamlAWSCredentials { sacAuthAccess :: !AWST.AccessKey
                     , sacAuthSecret :: !AWST.SecretKey
                     , sacAuthToken  :: !AWST.SessionToken
                     } deriving Eq

instance Show SamlAWSCredentials where
    show SamlAWSCredentials{..} = "SamlAWSCredentials{ sacAuthAccess = \"" <> show sacAuthAccess <>
                                                  "\", sacAuthSecret = \"" <> (show . AWSBS.toBS) sacAuthSecret <>
                                                  "\", sacAuthToken = \"" <> (show . AWSBS.toBS) sacAuthToken <>
                                                  "\" }"

data SamlAccountSession =
  SamlAccountSession { sasEmbedLink :: !OktaEmbedLink
                     , sasECRLogin :: !Bool
                     , sasSessionDurationSeconds :: !Natural
                     , sasAwsProfile :: !AWSProfile
                     , sasChosenSamlRole :: !(Maybe SamlRole)
                     , sasAwsCredentials :: !SamlAWSCredentials
                     , sasDockerAuths :: ![ECR.AuthorizationData]
                     } deriving (Eq, Show)

type SamlSession = [SamlAccountSession]


-- | SAML assertion (a Base64 encoded XML doc with SAML Response)
newtype SamlAssertion = SamlAssertion { unSamlAssertion :: Text } deriving (Eq, Show, FromJSON, ToJSON)

-- | Part of the SAML assertion payload, one of the available roles we can assume
data SamlRole =
  SamlRole { srRoleARN :: Text
           , srPrincipalARN :: Text
           } deriving (Eq, Show)


-- | Login with user credentials.
newtype AuthRequestUserCredentials =
  AuthRequestUserCredentials UserCredentials deriving (Eq, Show)


data AuthRequestMFAVerify =
    AuthRequestMFAPollVerify StateToken MFAFactorID
  | AuthRequestMFATOTPVerify StateToken MFAFactorID MFAPassCode
  | AuthRequestMFAPushVerify StateToken MFAFactorID RememberDevice AutoPush
  deriving (Eq, Show)

instance ToJSON AuthRequestMFAVerify where
  toJSON (AuthRequestMFAPollVerify (StateToken tok) (MFAFactorID fid)) =
    object [ "fid" .= fid
           , "stateToken" .= tok
           ]

  toJSON (AuthRequestMFATOTPVerify (StateToken tok) (MFAFactorID fid) (MFAPassCode pc)) =
    object [ "fid" .= fid
           , "stateToken" .= tok
           , "passCode" .= pc
           ]

  toJSON (AuthRequestMFAPushVerify (StateToken tok) (MFAFactorID fid) (RememberDevice remember) (AutoPush autoPush)) =
    object [ "fid" .= fid
           , "stateToken" .= tok
           , "rememberDevice" .= remember
           , "autoPush" .= autoPush
           ]


instance ToJSON AuthRequestUserCredentials where
  toJSON (AuthRequestUserCredentials (UserName u, Password p)) =
    object [ "username" .= u
           , "password" .= p
           , "options" .= object [ "multiOptionalFactorEnroll" .= False
                                 , "warnBeforePasswordExpired" .= False
                                 ]
           ]


-- | Enough fields to work with TOTP
-- (https://en.wikipedia.org/wiki/Time-based_One-time_Password_Algorithm)
-- factors, other factors are not supported for now
data MFAFactor =
  MFAFactor { mfaId :: MFAFactorID
            , mfaFactorType :: Text
            , mfaProvider :: Text
            } deriving (Eq, Show)

$(deriveJSON (aesonPrefix camelCase) ''MFAFactor)

data MFAFactorType = MFAFactorTOTP | MFAFactorPush deriving (Eq, Show)

instance FromText MFAFactorType where
  parser = takeLowerText >>= \case
    "totp" -> pure MFAFactorTOTP
    "push" -> pure MFAFactorPush
    e      ->
      fromTextError $ "Failure parsing MFA Factor to enroll from " <> e

instance ToText MFAFactorType where
  toText = \case
    MFAFactorTOTP -> "totp"
    MFAFactorPush -> "push"

data MFAFactorResult = MFAFactorRejected | MFAFactorWaiting deriving (Eq, Show)

instance FromJSON MFAFactorResult where
  parseJSON (String s) = case unpack s of
    "REJECTED" -> return MFAFactorRejected
    "WAITING"  -> return MFAFactorWaiting
    _          -> error "unkown result"

  parseJSON invalid = typeMismatch "MFAFactorResult" invalid


instance FromText MFAFactorResult where
  parser = takeLowerText >>= \case
    "rejected" -> pure MFAFactorRejected
    "waiting"  -> pure MFAFactorWaiting
    e          ->
      fromTextError $ "Failure parsing MFA factor resultfrom " <> e

-- | Subset of possible responses, just enough to loop in 2FA prompt.
data OktaAuthResponse =
    AuthResponseSuccess SessionToken
  | AuthResponseMFARequired StateToken [MFAFactor]
  | AuthResponseMFAChallenge StateToken MFAFactorResult MFAFactorID
  | AuthResponseOther Value
  deriving (Eq, Show)


instance FromJSON OktaAuthResponse where

  parseJSON v@(Object x) =
    do status <- x .: "status"
       case status of
         String "SUCCESS"       -> AuthResponseSuccess <$> x .: "sessionToken"
         String "MFA_REQUIRED"  -> AuthResponseMFARequired <$> x .: "stateToken" <*>
                                     return (Object x ^.. key "_embedded" . key "factors" . values . _JSON)
         String "MFA_CHALLENGE" -> AuthResponseMFAChallenge <$> x .: "stateToken" <*>
                                     x .: "factorResult" <*>
                                     return (Object x ^?! key "_embedded" . key "factor" . key "id" . _JSON)

         _                      -> return $ AuthResponseOther v


  parseJSON invalid = typeMismatch "OktaAuthResponse" invalid



-- | Non-200 responses
data OktaError =
    OktaErrorUnauthorized Value -- ^ 401 response
  | OktaErrorForbidden Value    -- ^ 403 response
  | OktaErrorOther Value        -- ^ generic unexpected response
  deriving (Eq, Show)



-- | To present user with choices
data InteractiveChoce a =
  InteractiveChoce { icKey :: Text
                   , icMessage :: Text
                   , icChoice :: a
                   } deriving (Eq, Show)
