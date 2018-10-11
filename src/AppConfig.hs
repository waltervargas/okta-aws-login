{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Company / account names / ids
module AppConfig (
  getEnvAWSProfile
, listAppConfigProfiles
, loadAppConfig
, mergeAppConfig
, newAppConfig
, tryReadAppConfig
, writeAppConfigFile
) where


import           Args
import           Control.Bool
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import qualified Data.Text as T
import           System.Directory
import           System.Environment
import           Types


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


-- | Reads app config file, either default or whichever is given on the command line
loadAppConfig :: Args
              -> IO (NonEmpty OktaAWSConfig)
loadAppConfig Args{..} = do
  appConf <- readAppConfigFile argsConfigFile
  envProf <- getEnvAWSProfile

  let defaultConfiguredProfiles = ocAwsProfile <$> NEL.filter (fromMaybe False . ocDefault) (unAppConfig appConf)

       -- consider profiles in the order of preference
      selectedProfiles = fromMaybe [] $ listToMaybe $ filter (not . null)
                           [ argsAwsProfiles
                           , maybeToList envProf
                           , defaultConfiguredProfiles
                           ]

      samlConfPredicate OktaAWSConfig{..} = ocAwsProfile `elem` selectedProfiles

      selectedSamlConfigs = NEL.filter samlConfPredicate (unAppConfig appConf)

  case NEL.nonEmpty selectedSamlConfigs
    of Nothing -> error $ "Please provide at least one AWS profile or specify default(s) (in the config file or via an AWS_PROFILE environmental variable)." <>
                          " You can re-run with -l to see the list of configured profiles."
       Just cs -> return cs


-- | Returns configured profiles, with an optional default configured profile
listAppConfigProfiles :: FilePath
                      -> IO ([AWSProfile], Maybe AWSProfile)
listAppConfigProfiles confFilePath = do
  AppConfig{..} <- readAppConfigFile confFilePath

  let isDefaultConfig = fromMaybe False . ocDefault

      maybeDefaultProfile = ocAwsProfile <$> find isDefaultConfig unAppConfig

      allProfiles = toList $ fmap ocAwsProfile unAppConfig

  return (allProfiles, maybeDefaultProfile)



-- | Reads required app config file, error if not found
readAppConfigFile :: FilePath
                  -> IO AppConfig
readAppConfigFile confFp = do
  ac <- tryReadAppConfig confFp
  case ac
    of Just x -> pure x
       Nothing -> error $ "Config file " <> confFp <>
                          " was not found or was not accessible, please configure first!"


-- | Looks up AWS_PROFILE env var
getEnvAWSProfile :: IO (Maybe AWSProfile)
getEnvAWSProfile = fmap (AWSProfile . T.pack) <$> lookupEnv "AWS_PROFILE"
