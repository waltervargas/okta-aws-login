{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


-- | Subset of the Okta API we need to login to AWS
-- http://developer.okta.com/docs/api/resources/authn.html

module OktaClient (
  findTotpFactors
, getOktaAWSSaml
, oktaAuthenticate
, oktaMFAVerify
) where


import           App
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Text.HTML.TagSoup
import           Types


findTotpFactors :: [MFAFactor]
                -> [MFAFactor]
findTotpFactors = filter (\MFAFactor{..} -> mfaFactorType == "token:software:totp")


-- | Makes a request to /authn end point
oktaAuthenticate :: OktaOrg
                 -> AuthRequestUserCredentials
                 -> App (Either OktaError OktaAuthResponse)
oktaAuthenticate oOrg = oktaPost oOrg "/api/v1/authn"


-- | Makes a request to /authn end point
oktaMFAVerify :: OktaOrg
              -> AuthRequestMFATOTPVerify
              -> App (Either OktaError OktaAuthResponse)
oktaMFAVerify oOrg req@(AuthRequestMFATOTPVerify _ (MFAFactorID fid) _) =
  oktaPost oOrg ("/api/v1/authn/factors/" <> fid <> "/verify") req


-- | Constructs a request object with given request path and mandatory headers
oktaPost :: (ToJSON reqBody, FromJSON resBody)
         => OktaOrg
         -> RequestPath
         -> reqBody
         -> App (Either OktaError resBody)
oktaPost oOrg rp rb = do
  initReq <- tParseRequest $ "https://" <> unOktaOrg oOrg <> ".okta.com" <> rp
  let req = initReq { method = "POST"
                    , requestHeaders = [ (hContentType, "application/json")
                                       , (hAccept,      "application/json")
                                       ]
                    , requestBody = RequestBodyLBS $ encode rb
                    }

  $(logDebug) $ "Okta request: " <> tshow req
  $(logDebug) $ "Okta request data: " <> (TE.decodeUtf8 . LB.toStrict . encode) rb
  res <- httpJSON req :: App (Response Value) -- get it as Value first, to print the whole thing for debugging
  $(logDebug) $ "Okta response: " <> tshow res

  let okResponse = case fromJSON (responseBody res)
                     of Error s -> error $ "Unable to parse model from " <> show res <> " error: " <> s
                        Success b -> return b

      errResponse r = return . Left  . r $ responseBody res

      chooseResponse s | s == ok200           = Right <$> okResponse
                       | s == unauthorized401 = errResponse OktaErrorUnauthorized
                       | s == forbidden403    = errResponse OktaErrorForbidden
                       | otherwise            = errResponse OktaErrorOther

  chooseResponse (responseStatus res)


-- | Gets SAML blob by executing a part of the browser request flow.
-- There's no clean API. One needs to make a sessionCookieRedirect request
-- to obtain 'sid' cookie and then call SAML integration page and parse HTML.
getOktaAWSSaml :: OktaOrg
               -> OktaAWSAccountID
               -> SessionToken
               -> App SamlAssertion
getOktaAWSSaml oOrg oAccId (SessionToken tok) = do

  -- This HTML is expected to contain
  -- <input name="SAMLResponse" type="hidden" value="
  let redirectUrl = "https://" <> unOktaOrg oOrg <> ".okta.com/app/amazon_aws/" <> unOktaAwsAccountId oAccId <> "/sso/saml?onetimetoken=" <> tok

  initReq <- tParseRequest $ "https://" <> unOktaOrg oOrg <> ".okta.com/login/sessionCookieRedirect"
  let req = initReq { queryString = renderSimpleQuery True [ ("token", TE.encodeUtf8 tok)
                                                           , ("redirectUrl", TE.encodeUtf8 redirectUrl)
                                                           ]
                    , requestHeaders = [ ("Accept", "*/*")
                                       , ("User-Agent", "curl/7.43.0") -- it's important to have a user agent, returns 404 without it
                                       ]
                    }

  $(logDebug) $ "Okta SAML request: " <> tshow req
  res <- httpLBS req
  $(logDebug) $ "Okta SAML response: " <> tshow res

  case parseSAMLResponseHTMLTag $ TE.decodeUtf8 . LB.toStrict . responseBody $ res
    of Nothing -> error $ "Unable to find SAMLResponse in the output of " <> show res
       Just x -> return $ SamlAssertion x


-- | Attempts to find
-- <input name="SAMLResponse" type="hidden" value="
-- in the response HTML and extract its value
parseSAMLResponseHTMLTag :: Text
                         -> Maybe Text
parseSAMLResponseHTMLTag htmlT =
  let allInputTagAttributes = fmap (\(TagOpen _ attrs) -> attrs) $ filter (isTagOpenName "input") $ parseTags htmlT
      samlResponseTag = listToMaybe $ filter (elem ("name", "SAMLResponse")) allInputTagAttributes
   in do samlTagAttrs <- samlResponseTag
         listToMaybe $ map snd $ filter (\(n,_) -> n == "value") samlTagAttrs



tParseRequest :: MonadThrow m
              => Text
              -> m Request
tParseRequest t = parseRequest (T.unpack t)
