{-# LANGUAGE OverloadedStrings          #-}

-- | Company / account names / ids
module AppConfig (
  defaultConfigFileName
, exampleAppConfig
, readAppConfigFile
) where


import           Control.Bool
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as U8
import           Data.List.NonEmpty
import           Data.Monoid
import           System.Directory
import           System.FilePath
import           Types


readAppConfigFile :: FilePath
                  -> IO AppConfig
readAppConfigFile confFp =
  ifThenElseM (doesFileExist confFp)
    ( do jsonData <- LB.readFile confFp
         case eitherDecode' jsonData
           of Left e -> error $ "Unable to parse JSON config from " <> confFp <> " error: " <> e
              Right c -> return c )
    ( error $ "Config file " <> confFp <> " was not found or was not accessible." )


defaultConfigFileName :: IO FilePath
defaultConfigFileName = do
  h <- getHomeDirectory
  return $ h </> ".okta-aws-login.json"


exampleAppConfig :: String
exampleAppConfig = U8.toString . encode . AppConfig $
  fromList [
    OktaSamlConfig "orgname" "my-aws-profile" "0oa1298hUiqWerSnBVpO" Nothing
  , OktaSamlConfig "orgname" "my-default-aws-profile" "0oa87GhDsxZaQw32571u" (Just True)
  ]
