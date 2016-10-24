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
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.AWS.ECR
import           Network.AWS.STS
import           System.IO
import           Types hiding (SessionToken)


awsAssumeRole :: SamlAssertion
              -> SamlRole
              -> App (Network.AWS.STS.Credentials, [AuthorizationData])
awsAssumeRole sa@(SamlAssertion samlAssertion) sr@SamlRole{..} = do
  v <- isVerbose
  r <- getAwsRegion
  lgr <- liftIO $ newLogger (if v then Debug else Info) stdout
  env <- liftIO $ newEnv r (FromKeys (AccessKey "xxx") (SecretKey "yyy")) <&> set envLogger lgr -- shouldn't need valid keys for this call

  stsCreds <- liftIO $ runResourceT . runAWST env $ do
    res <- send (assumeRoleWithSAML srRoleARN srPrincipalARN samlAssertion)
    case res ^. arwsamlrsCredentials
      of Just c -> return c
         Nothing -> error $ "Unable to get AWS credentials for " <> (show sa) <> " " <> (show sr)

  $(logDebug) $ T.pack $ "AWS credentials " <> (show stsCreds)

  let sessionCreds = FromSession ((AccessKey . TE.encodeUtf8)    (stsCreds ^. cAccessKeyId))
                                 ((SecretKey . TE.encodeUtf8)    (stsCreds ^. cSecretAccessKey))
                                 ((SessionToken . TE.encodeUtf8) (stsCreds ^. cSessionToken))

  env2 <- liftIO $ newEnv r sessionCreds <&> set envLogger lgr

  ecrAuthData <- liftIO $ runResourceT . runAWST env2 $ do
    res <- send getAuthorizationToken
    return $ res ^. gatrsAuthorizationData

  $(logDebug) $ T.pack $ "ECR auth data " <> (show ecrAuthData)

  return (stsCreds, ecrAuthData)
