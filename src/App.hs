{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App (
  App
, askUser
, chooseOne
, doECRLogin
, getArgs
, getAwsRegion
, getOktaAWSConfig
, getSamlSession
, getUserCredentials
, isVerbose
, keepReloading
, listAppConfigProfiles
, lookupChoice
, numericChoices
, runApp
, setSamlSession
, tshow
, updateSamlSession
, useMFA
, usedMFA
) where


import           AppConfig
import           Args
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.AWS.Types
import           System.IO
import           Types


data AppState =
  AppState { asArgs :: !Args
           , asOktaAWSConfig :: !(NonEmpty OktaAWSConfig)
           , asSamlSessionRef :: !(IORef SamlSession)
           , asUsedMFA :: !(IORef Bool) -- ^ remember if we had to use MFA codes during session (as opposed to being on a trusted net)
           }


newtype App a =
  App { unApp :: ReaderT AppState (LoggingT IO) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadCatch
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadThrow
                 , MonadReader AppState
                 )

runApp :: App a
       -> Args
       -> IO a
runApp appA args@Args{..} = do
  let llf _ ll = argsVerbose || (ll >= LevelInfo)

  samlConf <- loadAppConfig args
  samlSessRef <- newIORef []
  usedMFARef <- newIORef False

  let appState = AppState args samlConf samlSessRef usedMFARef
  runStderrLoggingT $ filterLogger llf $ runReaderT (unApp appA) appState


isVerbose :: App Bool
isVerbose = fmap argsVerbose getArgs

keepReloading :: App Bool
keepReloading = fmap argsKeepReloading getArgs

doECRLogin :: SamlAccountSession -> App Bool
doECRLogin SamlAccountSession{..} = do
  cliArg <- fmap argsECRLogin getArgs -- has precedence over config
  return $ fromMaybe sasECRLogin cliArg

getOktaAWSConfig :: App (NonEmpty OktaAWSConfig)
getOktaAWSConfig = App $ fmap asOktaAWSConfig ask


getUserCredentials :: App UserCredentials
getUserCredentials = getArgs >>= \Args{..} -> do
  uName <- case argsUserName
             of Just u -> return u
                Nothing -> UserName <$> askUser True "User > "
  password <- Password <$> askUser False "Please enter Okta password > "
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
getArgs = asks asArgs


getAwsRegion :: App Region
getAwsRegion = fmap argsRegion getArgs


getSamlSession :: App SamlSession
getSamlSession = do
  AppState{..} <- ask
  liftIO $ readIORef asSamlSessionRef

setSamlSession :: SamlSession
               -> App SamlSession
setSamlSession s = do
  AppState{..} <- ask
  liftIO $ writeIORef asSamlSessionRef s
  return s

updateSamlSession :: (SamlSession -> App SamlSession)
                  -> App SamlSession
updateSamlSession upS = getSamlSession >>= upS >>= setSamlSession


usedMFA :: App Bool
usedMFA = do
  AppState{..} <- ask
  liftIO $ readIORef asUsedMFA

useMFA :: Bool
       -> App ()
useMFA x = do
  AppState{..} <- ask
  liftIO $ writeIORef asUsedMFA x


tshow :: (Show a) => a -> Text
tshow = T.pack . show


-- | Interactive choices with numeric string keys
numericChoices :: [Text -> a -> InteractiveChoce a]
numericChoices = InteractiveChoce . tshow <$> ([0..] :: [Int])


-- | Lookup chosen value by key
lookupChoice :: Text -- ^ key
             -> [InteractiveChoce a]
             -> Maybe a
lookupChoice k cs =
  fmap icChoice $ listToMaybe $ filter (\InteractiveChoce{..} -> icKey == k) cs


-- | Ask user to pick an option among available choices
chooseOne :: NonEmpty (InteractiveChoce a)
          -> App a
chooseOne (InteractiveChoce{..} :| []) = return icChoice
chooseOne opts = do
  liftIO $ TIO.putStrLn $ T.intercalate "\n" $
             fmap (\InteractiveChoce{..} -> "[" <> icKey <> "] " <> icMessage)
               (NEL.toList opts)

  uc <- askUser True "Please choose> "

  case lookupChoice uc (NEL.toList opts)
    of Nothing -> do liftIO $ putStrLn $ "Sorry, your choice of " <> show uc <> " is not available, please try again."
                     chooseOne opts
       Just x -> return x


-- | Control terminal echo, flush stdin, for user interaction
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  hFlush stdout
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo)
           (do hSetEcho stdin old ; hFlush stdout)
           action
