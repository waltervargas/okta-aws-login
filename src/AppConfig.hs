{-# LANGUAGE OverloadedStrings          #-}

-- | Company / account names / ids
module AppConfig (
  defaultConfigFileName
, mergeAppConfig
, newAppConfig
, readAppConfigFile
, tryReadAppConfig
, writeAppConfigFile
) where


import           Control.Bool
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty as NEL
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Types


-- | Reads required app config file, error if not found
readAppConfigFile :: FilePath
                  -> IO AppConfig
readAppConfigFile confFp = do
  ac <- tryReadAppConfig confFp
  case ac
    of Just x -> pure x
       Nothing -> error $ "Config file " <> confFp <>
                          " was not found or was not accessible, please configure first!"


-- | Reads config file if present, returns Nothing otherwise
tryReadAppConfig :: FilePath
                 -> IO (Maybe AppConfig)
tryReadAppConfig confFp =
  ifThenElseM (doesFileExist confFp)
    ( do jsonData <- LB.readFile confFp
         case eitherDecode' jsonData
           of Left e -> error $ "Unable to parse JSON config from " <> confFp <> " error: " <> e
              Right c -> (return . Just) c )
    (return Nothing)


-- | Writes app config out to file
writeAppConfigFile :: FilePath
                   -> AppConfig
                   -> IO ()
writeAppConfigFile confFp appConf =
  LB.writeFile confFp (encodePretty appConf)


-- | Merges new config section into existing app config
mergeAppConfig :: OktaAWSConfig
               -> AppConfig
               -> AppConfig
mergeAppConfig oConf appConf = AppConfig $
  oConf :| (maybeResetDefault . filterOutExisting . unAppConfig) appConf

  where filterOutExisting = NEL.filter (\c -> ocAwsProfile c /= ocAwsProfile oConf)
        maybeResetDefault xs = if fromMaybe False (ocDefault oConf)
                                 then fmap (\c -> c { ocDefault = Just False }) xs
                                 else xs

-- | Creates new app config
newAppConfig :: OktaAWSConfig
             -> AppConfig
newAppConfig oConf = AppConfig $ oConf :| []


-- | Constructs user-default config path
defaultConfigFileName :: IO FilePath
defaultConfigFileName = do
  h <- getHomeDirectory
  return $ h </> ".okta-aws-login.json"
