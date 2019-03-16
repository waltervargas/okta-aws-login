{-# LANGUAGE TemplateHaskell    #-}

module Args (
  Args(..)
, Command(..)
, ConfigArgs(..)
, parseCLICommand
) where


import           Data.Functor
import qualified Data.Text as T
import           Development.GitRev
import           Network.AWS.Data
import           Network.AWS.Prelude (Natural)
import           Network.AWS.Types
import           Options.Applicative
import           System.Directory
import           System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Types


-- | Arguments used for main (login to AWS) functionality
data Args = Args { argsVerbose :: !Bool
                 , argsUserName :: !(Maybe UserName)
                 , argsAwsProfiles :: ![AWSProfile]
                 , argsRegion :: !Region
                 , argsConfigFile :: !FilePath
                 , argsKeepReloading :: !Bool
                 , argsMfaFactor :: !MFAFactorType
                 , argsECRLogin :: !(Maybe Bool)
                 } deriving (Show)


-- | Arguments used to initially configure this tool
data ConfigArgs = ConfigArgs { confArgsProfile :: !AWSProfile
                             , confArgsOktaEmbedLink :: !OktaEmbedLink
                             , confArgsIsDefaultProfile :: !(Maybe Bool)
                             , confArgsEnableECRLogin :: !(Maybe Bool)
                             , confArgsConfFilePath :: !FilePath
                             , confArgsSessionDurationSeconds :: !Natural
                             } deriving Show


-- | Top-level command, either proceed with login (most likely)
--   configure this tool (initially)
data Command = Configure ConfigArgs
             | ListProfiles FilePath
             | Login Args
             deriving Show


parseArgs :: FilePath
          -> Parser Args
parseArgs defConf =
  (infoOption ("Version: " <> $(gitBranch) <> "@" <> $(gitHash))
              (long "version" <> short 'V' <> help "Print version and exit.") <*> pure Args)
  <*> switch
      ( long "verbose"
    <> short 'v'
    <> help "Be verbose.")
  <*> optional (UserName . T.pack <$> strOption
      ( long "user"
    <> short 'u'
    <> help "User name." ))
  <*> many (AWSProfile . T.pack <$> strOption
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
  <*> option parseMfaFactor
      ( long "mfa-factor"
    <> short 'f'
    <> value MFAFactorTOTP
    <> showDefaultWith (T.unpack .toText)
    <> help "MFA Factor." )
  <*> (
        flag Nothing (Just False) (long "no-ecr" <> help "Skip Docker login to ECR registry, default is in the config.")
        <|>
        flag Nothing (Just True) (long "ecr" <> help "Attempt Docker login to ECR registry, default is in the config.")
      )


parseRegion :: ReadM Region
parseRegion = eitherReader (fromText . T.pack)

parseMfaFactor :: ReadM MFAFactorType
parseMfaFactor = eitherReader (fromText . T.pack)


parseConfigArgs :: FilePath
                -> Parser ConfigArgs
parseConfigArgs defConf = ConfigArgs
  <$> (AWSProfile . T.pack <$> strOption
        ( long "aws-profile"
      <> short 'p'
      <> help "Name of the associated AWS profile."))
  <*> (OktaEmbedLink . T.pack <$> strOption
        ( long "okta-embed-link"
      <> short 'l'
      <> help "Okta AWS app 'embed' link, ask your Okta administrator."))
  <*> optional ( switch
        (long "default"
        <> short 'd'
        <> help "User this profile by default."))
  <*> optional (switch
        (long "ecr"
        <> short 'r'
        <> help "Enable Docker login to ECR registry by default."))
  <*> strOption
      ( long "config-file"
    <> short 'c'
    <> value defConf
    <> showDefaultWith show
    <> help "Use alternative config file." )
  <*> option auto
        ( long "session-duration"
      <> short 's'
      <> help "STS session duration, seconds. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. Please coordinate with your AWS administrator."
        )


parseCommand :: FilePath -- ^ default config file
             -> Parser Command
parseCommand defConf =
  hsubparser (command "configure"
             (info (Configure <$> parseConfigArgs defConf)
                   (progDesc "Configure AWS profile given an Okta 'embed' link")))
  <|>
  Login <$> parseArgs defConf
  <|>
  ( switch ( long "list-profiles" <> short 'l' <> help "List available AWS profiles and exit.")
    $> ListProfiles defConf
  )


-- | Constructs user-default config path
defaultConfigFileName :: IO FilePath
defaultConfigFileName = do
  h <- getHomeDirectory
  return $ h </> ".okta-aws-login.json"


parseCLICommand :: IO Command
parseCLICommand = do
  defConfFile <- defaultConfigFileName
  execParser (opts defConfFile)
  where
    opts defConf = info (helper <*> parseCommand defConf)
      ( fullDesc
     <> header "Login to AWS via Okta/SAML."
     <> progDesc ( "Login to AWS via Okta/SAML " <>
                   " (source: https://github.com/saksdirect/okta-aws-login) " <>
                   " Default config file: " <> show defConf
                 )
     <> (footerDoc . Just)
        (PP.text "Log in using default AWS profile, you'll be prompted for user name / password:"
         PP.<+> PP.linebreak
         PP.<$> PP.indent 2 (PP.text "$ okta-aws-login")
         PP.<+> PP.linebreak
         PP.<$> PP.text "Specify user name and keep reloading session:"
         PP.<+> PP.linebreak
         PP.<$> PP.indent 2 (PP.text "$ okta-aws-login --user my-okta-user-name --keep-reloading")
         PP.<+> PP.linebreak
         PP.<$> PP.text "Log in with more than one AWS profile:"
         PP.<+> PP.linebreak
         PP.<$> PP.indent 2 (PP.text "$ okta-aws-login --user my-okta-user-name --aws-profile my-aws-profile1 --aws-profile my-aws-profile2")
         PP.<+> PP.linebreak
         PP.<$> PP.text "Skip ECR login (note that you can set default behavior in the config file)"
         PP.<+> PP.linebreak
         PP.<$> PP.indent 2 (PP.text "$ okta-aws-login --no-ecr --user my-okta-user-name --aws-profile my-aws-profile1")
        )
      )
