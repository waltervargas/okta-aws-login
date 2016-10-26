{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module AWSCredsFile (
  updateAwsCreds
) where


import           App
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.HashMap.Strict as M
import           Data.Ini
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Data.Text
import           Network.AWS.STS
import           System.Directory
import           System.FilePath
import           Types


updateAwsCreds :: Credentials
               -> App ()
updateAwsCreds creds = do
  credsFile <- awsCredentialsFileName

  createConfFileIfDoesntExist credsFile "[default]"

  OktaSamlConfig{..} <- getOktaSamlConfig

  !savedCreds <- liftIO $ readIniFile credsFile >>=
                   \x -> case x
                           of Left e -> error $ "Unable to read AWS credentials file from " <> credsFile <> " error: " <> e
                              Right c -> return $ unIni c

  region <- getAwsRegion

  let profileSection = unAwsProfile ocAwsProfile
      savedProfileConfSection = fromMaybe M.empty $ M.lookup profileSection savedCreds

      updatedCreds =
        M.insert profileSection
                 (((M.insert "region"                (toText region)) .
                   (M.insert "aws_access_key_id"     (creds ^. cAccessKeyId)) .
                   (M.insert "aws_secret_access_key" (creds ^. cSecretAccessKey)) .
                   (M.insert "aws_session_token"     (creds ^. cSessionToken)) .
                   (M.insert "aws_security_token"    (creds ^. cSessionToken)) ) savedProfileConfSection )
                 savedCreds

  $(logDebug) $ "Writing new credentials to file " <> (tshow updatedCreds)
  liftIO $ writeIniFileWith (WriteIniSettings EqualsKeySeparator) credsFile (Ini updatedCreds)


awsCredentialsFileName :: App FilePath
awsCredentialsFileName = liftIO $ do
  h <- getHomeDirectory
  return $ h </> ".aws" </> "credentials"
