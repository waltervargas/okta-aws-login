{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module AWSCredsFile (
  updateAwsCreds
) where


import           App
import           Control.Bool
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.HashMap.Strict as M
import           Data.Ini
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Network.AWS.Data.Text
import           Network.AWS.STS
import           System.Directory
import           System.FilePath
import           Types


updateAwsCreds :: Credentials
               -> App ()
updateAwsCreds creds = do
  createCredsFileIfDoesntExist

  OktaSamlConfig{..} <- getOktaSamlConfig

  credsFile <- awsCredentialsFileName
  !savedCreds <- liftIO $ readIniFile credsFile >>=
                   \x -> case x
                           of Left e -> error $ "Unable to read AWS credentials file from " <> credsFile <> " error: " <> e
                              Right c -> return $ unIni c

  region <- getAwsRegion

  let profileSection = (T.pack . unAwsProfile) ocAwsProfile
      savedProfileConfSection = fromMaybe M.empty $ M.lookup profileSection savedCreds

      updatedCreds =
        M.insert profileSection
                 (((M.insert "region"                (toText region)) .
                   (M.insert "aws_access_key_id"     (creds ^. cAccessKeyId)) .
                   (M.insert "aws_secret_access_key" (creds ^. cSecretAccessKey)) .
                   (M.insert "aws_session_token"     (creds ^. cSessionToken)) .
                   (M.insert "aws_security_token"    (creds ^. cSessionToken)) ) savedProfileConfSection )
                 savedCreds

  $(logDebug) $ T.pack $  "Writing new credentials to file " <> (show updatedCreds)
  liftIO $ writeIniFile credsFile (Ini updatedCreds)


awsCredentialsFileName :: App FilePath
awsCredentialsFileName = liftIO $ do
  h <- getHomeDirectory
  return $ h </> ".aws" </> "credentials"


-- | Needs to be created before we make any AWS calls
createCredsFileIfDoesntExist :: App ()
createCredsFileIfDoesntExist = do
  fn <- awsCredentialsFileName
  let (awsDir, _) = splitFileName fn

  created <- liftIO $
    ifThenElseM (doesFileExist fn)
        (return False)
        (do createDirectoryIfMissing False awsDir
            writeFile fn "[default]"
            return True)

  when created $ $(logInfo) $ T.pack $ "Created default AWS settings " <> fn
