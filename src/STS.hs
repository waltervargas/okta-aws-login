{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


-- | Interacts with AWS secure token service

module STS (
  awsAssumeRole
) where

import           App
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.AWS
import qualified Data.Text as T
import           Network.AWS.ECR
import           Network.AWS.STS
import           System.IO
import           Types hiding (SessionToken)

awsAssumeRole :: SamlAssertion
              -> SamlRole
              -> App (SamlAWSCredentials, [AuthorizationData])
awsAssumeRole sa@(SamlAssertion samlAssertion) sr@SamlRole{..} = do
  v <- isVerbose
  r <- getAwsRegion
  lgr <- newLogger (if v then Debug else Info) stdout
  env <- newEnv (FromKeys (AccessKey "xxx") (SecretKey "yyy")) <&> set envLogger lgr . set envRegion r -- shouldn't need valid keys for this call

  stsCreds <- liftIO $ runResourceT . runAWST env $ do
    res <- send (assumeRoleWithSAML srRoleARN srPrincipalARN samlAssertion)
    case res ^. arwsamlrsCredentials
      of Just c -> return c
         Nothing -> error $ "Unable to get AWS credentials for " <> show sa <> " " <> show sr

  $(logDebug) $ T.pack $ "AWS credentials " <> show stsCreds

  sessTok <- case stsCreds ^. sessionToken
               of Just t -> return t
                  Nothing -> error $ "Unable to get AWS session token for " <> show sa <> " " <> show sr

  let sessionCreds = FromSession (stsCreds ^. accessKeyId)
                                 (stsCreds ^. secretAccessKey)
                                 sessTok

  env2 <- newEnv sessionCreds <&> set envLogger lgr . set envRegion r

  ecrAuthData <- liftIO $ runResourceT . runAWST env2 $ do
    res <- send getAuthorizationToken
    return $ res ^. gatrsAuthorizationData

  $(logDebug) $ T.pack $ "ECR auth data " <> show ecrAuthData

  let sawsc = SamlAWSCredentials (stsCreds ^. accessKeyId)
                                 (stsCreds ^. secretAccessKey)
                                 sessTok

  return (sawsc, ecrAuthData)
