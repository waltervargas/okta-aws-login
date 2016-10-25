{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}

module Args (
  Args(..)
, runWithArgs
) where


import           AppConfig
import           Data.Monoid
import qualified Data.Text as T
import           Network.AWS.Data
import           Network.AWS.Types
import           Options.Applicative
import           Types


data Args = Args { argsVerbose :: !Bool
                 , argsUserName :: !(Maybe UserName)
                 , argsAwsProfile :: !(Maybe AWSProfile)
                 , argsRegion :: !Region
                 , argsConfigFile :: !FilePath
                 , argsKeepReloading :: !Bool
                 } deriving (Show)


parseArgs :: FilePath
          -> Parser Args
parseArgs defConf = Args
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "Be verbose.")
     <*> (optional $ fmap (UserName . T.pack) $ strOption
         ( long "user"
        <> short 'u'
        <> help "User name." ))
     <*> (optional $ fmap (AWSProfile . T.pack) $ strOption
         ( long "aws-profile"
        <> short 'p'
        <> help "AWS profile. Default config entry will be used if not given." ))
     <*> (option parseRegion
         ( long "region"
        <> short 'r'
        <> value NorthVirginia
        <> showDefaultWith (T.unpack . toText)
        <> help "AWS region." ))
     <*> (strOption
         ( long "config-file"
        <> short 'c'
        <> value defConf
        <> showDefaultWith show
        <> help "Use alternative config file." ))
     <*> switch
         ( long "keep-reloading"
        <> short 'k'
        <> help "Keep reloading session token hourly (that's the max TTL at the moment). This only works well on a trusted network where you don't need MFA.")


parseRegion :: ReadM Region
parseRegion = eitherReader (fromText . T.pack)


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = do
  defConfFile <- defaultConfigFileName
  execParser (opts defConfFile) >>= rwa
  where
    opts defConf = info (helper <*> (parseArgs defConf))
      ( fullDesc
     <> header "Login to AWS via Okta/SAML."
     <> progDesc ( "Login to AWS via Okta/SAML " <>
                   " (source: https://github.com/andreyk0/okta-aws-login) " <>
                   " Default config file: " <> (show defConf) <>
                   " Example config JSON: " <> exampleAppConfig
                 )
      )
