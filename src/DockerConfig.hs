{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module DockerConfig (
  updateDockerConfig
) where


import           App
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson as A
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Data.Text
import           Network.AWS.ECR
import           System.Directory
import           System.FilePath


updateDockerConfig :: [AuthorizationData]
                   -> App ()
updateDockerConfig [] = return ()
updateDockerConfig ads = do

  cfName <- dockerConfFileName

  createConfFileIfDoesntExist cfName "{}"

  !conf <- fromMaybe (object []) . decode <$> liftIO (LB.readFile cfName)

  let validAuths = catMaybes $ fmap authSection ads
      atKey k = _Object . at k
      appendAuth (pep,dauth) c = c & atKey "auths" . non (Object mempty) . atKey pep . non (Object mempty) .~ dauth
      newConf = foldr appendAuth conf validAuths

  $(logDebug) $ "Valid docker auth data: " <> tshow validAuths
  $(logDebug) $ "Updating docker conf to: " <> tshow newConf

  liftIO $ LB.writeFile cfName (encode newConf)


authSection :: AuthorizationData
            -> Maybe (Text, Value)
authSection ad = do
  proxyEp <- ad ^. adProxyEndpoint
  authTok <- ad ^. adAuthorizationToken
  return (proxyEp, object [ "email" A..= ("none" :: Text)
                          , "auth"  A..= authTok ])


dockerConfFileName :: App FilePath
dockerConfFileName = liftIO $ do
  h <- getHomeDirectory
  return $ h </> ".docker" </> "config.json"
