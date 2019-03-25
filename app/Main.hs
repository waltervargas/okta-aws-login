{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where


import           App
import           AppConfig
import           Args
import           Control.Monad
import           Data.Maybe
import qualified Data.Text.IO as TIO
import           OktaLogin
import           System.Exit
import           Types

main:: IO ()
main = parseCLICommand >>= runTopLevelCommand


-- | Run top-level command
runTopLevelCommand :: Command -> IO ()


-- | List configured AWS profiles
runTopLevelCommand (ListProfiles confFilePath) = do
  (allProfiles, defProf) <- listAppConfigProfiles confFilePath
  envProf <- getEnvAWSProfile

  forM_ allProfiles $ \p -> TIO.putStrLn $ unAwsProfile p
  forM_ defProf $ \p -> TIO.putStrLn $ "\nYour configured default profile is " <> unAwsProfile p <> "."
  forM_ envProf $ \p -> TIO.putStrLn $ "\nYour environment is configured with " <> unAwsProfile p <> ", it will override your config file defaults."

  when ((null . catMaybes) [envProf, defProf]) $
    TIO.putStrLn $ "\nNote that you can change your default AWS profile by exporting AWS_PROFILE environmental variable" <>
                    " or adding '\"default\": true' property to your preferred AWS profile in the config file."

  die ""


-- | Initial (or incremental) config of the app
runTopLevelCommand (Configure ConfigArgs{..}) = do
  let newConfSection = OktaAWSConfig confArgsOktaEmbedLink confArgsProfile
                                     confArgsIsDefaultProfile confArgsEnableECRLogin
                                     confArgsSessionDurationSeconds

  maybeConf <- tryReadAppConfig confArgsConfFilePath
  let newConf = case maybeConf
                  of Nothing -> newAppConfig newConfSection
                     Just ac -> mergeAppConfig newConfSection ac

  writeAppConfigFile confArgsConfFilePath newConf


-- | Main top-level command
runTopLevelCommand (Login args) = runApp oktaLogin args
