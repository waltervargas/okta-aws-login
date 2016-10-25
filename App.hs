{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module App (
  App
, askUser
, chooseOne
, createConfFileIfDoesntExist
, getAwsRegion
, getOktaSamlConfig
, getUserCredentials
, isVerbose
, keepReloading
, lookupChoice
, numericChoices
, runApp
, tshow
) where


import           AppConfig
import           Args
import           Control.Bool
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.List.NonEmpty as NL
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.AWS.Types
import           System.Directory
import           System.FilePath
import           System.IO
import           Types


newtype App a =
  App { unApp :: ReaderT (Args, OktaSamlConfig) (LoggingT IO) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadThrow
                 )

runApp :: App a
       -> Args
       -> IO a
runApp appA args@Args{..} = do
  samlConf <- findOktaSamlConfig args
  let llf _ ll = if argsVerbose then True else (ll >= LevelInfo)

  runStderrLoggingT $ filterLogger llf $ runReaderT (unApp appA) (args, samlConf)


isVerbose :: App Bool
isVerbose = fmap argsVerbose getArgs

keepReloading :: App Bool
keepReloading = fmap argsKeepReloading getArgs

getOktaSamlConfig :: App OktaSamlConfig
getOktaSamlConfig = App $ fmap snd ask


getUserCredentials :: App UserCredentials
getUserCredentials = getArgs >>= \Args{..} -> do
  uName <- case argsUserName
             of Just u -> return u
                Nothing -> UserName <$> (askUser True "User > ")
  password <- Password <$> (askUser False "Please enter Okta password > ")
  return (uName, password)


askUser :: Bool -- ^ terminal echo
        -> Text -- ^ prompt string
        -> App Text -- ^ user input
askUser e p = liftIO $ withEcho e $ do
  TIO.putStr p
  hFlush stdout
  r <- TIO.getLine
  TIO.putStrLn ""
  return r


getArgs :: App Args
getArgs = App $ fmap fst ask


getAwsRegion :: App Region
getAwsRegion = fmap argsRegion getArgs


tshow :: (Show a) => a -> Text
tshow = T.pack . show


-- | Interactive choices with numeric string keys
numericChoices :: [(String -> a -> InteractiveChoce a)]
numericChoices = (InteractiveChoce . show) <$> ([0..] :: [Int])


-- | Lookup chosen value by key
lookupChoice :: String -- ^ key
             -> [InteractiveChoce a]
             -> Maybe a
lookupChoice k cs =
  fmap icChoice $ listToMaybe $ filter (\InteractiveChoce{..} -> icKey == k) cs



chooseOne :: NonEmpty (InteractiveChoce a)
          -> App a
chooseOne (InteractiveChoce{..} :| []) = return icChoice
chooseOne opts = do
  liftIO $ putStrLn $ intercalate "\n" $
             fmap (\InteractiveChoce{..} -> "[" <> icKey <> "] " <> icMessage)
               (NL.toList opts)

  uc <- askUser True "Please choose> "

  case lookupChoice (T.unpack uc) (NL.toList opts)
    of Nothing -> do liftIO $ putStrLn $ "Sorry, your choice of " <> (show uc) <> " is not available, please try again."
                     chooseOne opts
       Just x -> return x


-- | Creates a missing config file with a default text
createConfFileIfDoesntExist :: FilePath
                            -> Text
                            -> App ()
createConfFileIfDoesntExist fp txt = do
  let (dir, _) = splitFileName fp

  created <- liftIO $
    ifThenElseM (doesFileExist fp)
      (return False)
      ( do createDirectoryIfMissing False dir
           TIO.writeFile fp txt
           return True)

  when created $ $(logInfo) $ "Created default config " <> (T.pack fp)



findOktaSamlConfig :: Args
                   -> IO OktaSamlConfig
findOktaSamlConfig Args{..} = do
  appConf <- readAppConfigFile argsConfigFile

  let acctPredicate = case argsAwsProfile
                        of Nothing -> (fromMaybe False) . ocDefault
                           Just a -> \osc -> a == (ocAwsProfile osc)

  case listToMaybe $ NEL.filter acctPredicate (ocSaml appConf)
    of Nothing -> error $ "Couldn't find specified AWS profile, please make sure there's a config for it or there's a default profile in a config. Configured profiles are: " <> (show $ fmap ocAwsProfile (ocSaml appConf))
       Just c -> return c



-- | Control terminal echo, flush stdin, for user interaction
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  hFlush stdout
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo)
           (do hSetEcho stdin old ; hFlush stdout)
           action
