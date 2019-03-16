{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


-- | Subset of the Okta API we need to login to AWS
-- http://developer.okta.com/docs/api/resources/authn.html

module OktaClient (
  findTotpFactors
, findPushFactors
, getOktaAWSSaml
, oktaAuthenticate
, oktaMFAVerify
, parseOktaOrg
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


findFactors :: Text
            -> [MFAFactor]
            -> [MFAFactor]
findFactors prefix factors = filter (\MFAFactor{..} -> T.isPrefixOf prefix mfaFactorType) factors

findTotpFactors :: [MFAFactor]
                -> [MFAFactor]
findTotpFactors = findFactors "token"

findPushFactors :: [MFAFactor]
                -> [MFAFactor]
findPushFactors = findFactors "push"

-- | Makes a request to /authn end point
oktaAuthenticate :: OktaOrg
                 -> AuthRequestUserCredentials
                 -> App (Either OktaError OktaAuthResponse)
oktaAuthenticate oOrg = oktaPost oOrg "/api/v1/authn"


-- | Makes a request to /authn end point
oktaMFAVerify :: OktaOrg
              -> AuthRequestMFAVerify
              -> App (Either OktaError OktaAuthResponse)
oktaMFAVerify oOrg req@(AuthRequestMFAPollVerify _ (MFAFactorID fid)) =
  oktaPost oOrg ("/api/v1/authn/factors/" <> fid <> "/verify") req
oktaMFAVerify oOrg req@(AuthRequestMFATOTPVerify _ (MFAFactorID fid) _) =
  oktaPost oOrg ("/api/v1/authn/factors/" <> fid <> "/verify") req
oktaMFAVerify oOrg req@(AuthRequestMFAPushVerify _ (MFAFactorID fid) _ _) =
  oktaPost oOrg ("/api/v1/authn/factors/" <> fid <> "/verify") req


-- | Constructs a request object with given request path and mandatory headers
oktaPost :: (ToJSON reqBody, FromJSON resBody)
         => OktaOrg
         -> RequestPath
         -> reqBody
         -> App (Either OktaError resBody)
oktaPost oOrg rp rb = do
  initReq <- tParseRequest $ "https://" <> unOktaOrg oOrg <> rp
  let req = initReq { method = "POST"
                    , requestHeaders = [ (hContentType, "application/json")
                                       , (hAccept,      "application/json")
                                       ]
                    , requestBody = RequestBodyLBS $ encode rb
                    }

  $(logDebug) $ "Okta request: " <> tshow req
  $(logDebug) $ "Okta request data: " <> (TE.decodeUtf8 . LB.toStrict . encode) rb
  res <- httpJSON req :: App (Response Value) -- get it as Value first, to print the whole thing for debugging
  $(logDebug) $ printHTTPBinaryResponse (TE.decodeUtf8 . LB.toStrict . encode) res

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
getOktaAWSSaml :: OktaEmbedLink
               -> SessionToken
               -> App SamlAssertion
getOktaAWSSaml oel@(OktaEmbedLink oeLink) (SessionToken tok) = do

  oktaOrg <- parseOktaOrg oel

  -- This HTML is expected to contain
  -- <input name="SAMLResponse" type="hidden" value="
  let tokenizedEmbedLink = TE.encodeUtf8 $ oeLink <> "?onetimetoken=" <> tok

  initReq <- tParseRequest $ "https://" <> unOktaOrg oktaOrg <> "/login/sessionCookieRedirect"

  let req = initReq { queryString = renderSimpleQuery True [ ("token", TE.encodeUtf8 tok)
                                                           , ("redirectUrl", tokenizedEmbedLink)
                                                           ]
                    , requestHeaders = [ ("Accept", "*/*")
                                       , ("User-Agent", "curl/7.43.0") -- it's important to have a user agent, returns 404 without it
                                       ]
                    }

  $(logDebug) $ "Okta SAML request: " <> tshow req
  res <- httpLBS req
  $(logDebug) $ printHTTPBinaryResponse (TE.decodeUtf8 . LB.toStrict) res

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


printHTTPBinaryResponse :: (a -> Text) -> Response a -> Text
printHTTPBinaryResponse bodyText res =
  "Okta response:\n\tSTATUS: " <> tshow (responseStatus res) <>
  "\n\tHEADERS: " <> tshow (responseHeaders res) <>
  "\n\tCOOKIES: " <> tshow (responseCookieJar res) <>
  "\n\tBODY:\n" <> (bodyText . responseBody) res


-- | AWS app "embed" links are always scoped to a registered organization,
--   we can simply extract the domain part here
parseOktaOrg :: OktaEmbedLink -> App OktaOrg
parseOktaOrg (OktaEmbedLink el) = OktaOrg . TE.decodeUtf8 . host <$> parseRequest (T.unpack el)
