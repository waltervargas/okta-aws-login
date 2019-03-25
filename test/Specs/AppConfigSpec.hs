{-# LANGUAGE OverloadedStrings #-}

module Specs.AppConfigSpec
  ( spec
  ) where


import           AppConfig
import qualified Data.List.NonEmpty as NEL
import           TestImports
import           Types


spec :: Spec
spec =
  describe "AppConfig" $ do
    it "Can read old JSON config format" testTryReadOldAppConfig
    it "Can read current JSON config format" testTryReadCurrentAppConfig
    it "Can write/read current JSON config format" testCurrentAppConfig


-- See discussion: https://github.com/saksdirect/okta-aws-login/pull/11
testTryReadOldAppConfig :: Expectation
testTryReadOldAppConfig = do
  conf <- tryReadAppConfig "data/test-config-old-json-format.json"
  conf `shouldBe` Just egConfig


-- If JSON encoding flips again by accident this should catch it
testTryReadCurrentAppConfig :: Expectation
testTryReadCurrentAppConfig = do
  conf <- tryReadAppConfig "data/test-config-current-json-format.json"
  conf `shouldBe` Just egConfig

-- write test config and read it back in
testCurrentAppConfig :: Expectation
testCurrentAppConfig = do
  let confPath = "/tmp/okta-aws-login-app-config-spec.json"
  writeAppConfigFile confPath egConfig
  conf <- tryReadAppConfig confPath
  conf `shouldBe` Just egConfig


egConfig :: AppConfig
egConfig = AppConfig $ NEL.fromList
  [ OktaAWSConfig
    { ocEmbedLink = OktaEmbedLink "https://blah.blah.com/home/amazon_aws/1"
    , ocAwsProfile = AWSProfile "test1"
    , ocDefault = Just True
    , ocECRLogin = Just True
    , ocSessionDurationSeconds = 43200
    }
  , OktaAWSConfig
    { ocEmbedLink = "https://blah.blah.com/home/amazon_aws/2"
    , ocAwsProfile = AWSProfile "test2"
    , ocDefault = Just False
    , ocECRLogin =Just False
    , ocSessionDurationSeconds = 43200
    }
  , OktaAWSConfig
    { ocEmbedLink = "https://blah.blah.com/home/amazon_aws/3"
    , ocAwsProfile = AWSProfile "test3"
    , ocDefault = Nothing
    , ocECRLogin = Nothing
    , ocSessionDurationSeconds = 12345
    }
  ]
