{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module DockerLogin (
  dockerLogin
) where


import           App
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Conversions
import qualified Data.Text.IO as TIO
import           Network.AWS.ECR
import           System.Exit
import           System.IO
import           System.Process



-- https://github.com/docker/for-mac/issues/1439

dockerLogin :: [AuthorizationData]
            -> App ()
dockerLogin ads =
  catch (mapM_ dockerLoginWith ads)
        (\ (SomeException e) -> $(logError) $ "Failed to run 'docker login': " <> tshow e <> ", if you don't care about ECR please re-run with --no-ecr flag!" )


dockerLoginWith :: AuthorizationData
                -> App ()
dockerLoginWith ad = do
  let dAuth = do
        authTok <- ad ^. adAuthorizationToken
        proxyEp <- ad ^. adProxyEndpoint
        tokBytes <- fromText authTok :: Maybe (Base64 LB.ByteString)
        (dUser, dPasswd) <- decodeEcrAuthTok tokBytes

        return (dUser, dPasswd, proxyEp)

  $(logDebug) $ "Docker auth data: " <> tshow dAuth

  forM_ dAuth $ \(u,p,e) -> runDockerLoginCmd u p e



runDockerLoginCmd :: Text
                  -> Text
                  -> Text
                  -> App ()
runDockerLoginCmd dUser dPasswd ecrEp = do
  ec <- liftIO $ withCreateProcess ( proc "docker" ["login", "-u", fromText dUser, "--password-stdin", fromText ecrEp]
                                   ){ std_in = CreatePipe }  $ \(Just hin) _ _ ph -> do
    TIO.hPutStr hin dPasswd
    hClose hin
    waitForProcess ph

  case ec
    of ExitSuccess   -> $(logInfo) $ "Successfully logged in to ECR " <> ecrEp
       ExitFailure x -> $(logError) $ "Failed to log in to ECR " <> ecrEp <> ", docer exit code: " <> tshow x



-- | Decodes user / password from a base64 encoded ECR auth token
decodeEcrAuthTok :: Base64 LB.ByteString
                 -> Maybe (Text, Text)
decodeEcrAuthTok (Base64 tokBytes) = do
  tok <- decodeText (UTF8 tokBytes)

  case T.split (== ':') (toText tok)
    of [u,p] -> Just (u, p)
       _     -> Nothing
