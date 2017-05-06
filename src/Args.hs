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
                 , argsVersion :: !Bool
                 , argsListAwsProfiles :: !Bool
                 , argsUserName :: !(Maybe UserName)
                 , argsAwsProfiles :: ![AWSProfile]
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
     <*> switch
         ( long "version"
        <> short 'V'
        <> help "Print version and exit.")
     <*> switch
         ( long "list-profiles"
        <> short 'l'
        <> help "List available AWS profiles and exit.")
     <*> optional ((UserName . T.pack) <$> strOption
         ( long "user"
        <> short 'u'
        <> help "User name." ))
     <*> many ((AWSProfile . T.pack) <$> strOption
         ( long "aws-profile"
        <> short 'p'
        <> help "AWS profile. Defaults to value of AWS_PROFILE env var, then to default config entry."))
     <*> option parseRegion
         ( long "region"
        <> short 'r'
        <> value NorthVirginia
        <> showDefaultWith (T.unpack . toText)
        <> help "AWS region." )
     <*> strOption
         ( long "config-file"
        <> short 'c'
        <> value defConf
        <> showDefaultWith show
        <> help "Use alternative config file." )
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
    opts defConf = info (helper <*> parseArgs defConf)
      ( fullDesc
     <> header "Login to AWS via Okta/SAML."
     <> progDesc ( "Login to AWS via Okta/SAML " <>
                   " (source: https://github.com/andreyk0/okta-aws-login) " <>
                   " Default config file: " <> show defConf <>
                   " Example config JSON: " <> exampleAppConfig
                 )
      )
