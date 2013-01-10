{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Github.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec.ByteString.Lazy
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Types (Method, Status(..))
import Network.HTTP.Conduit
import Data.Conduit (ResourceT)
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)

githubGet :: (FromJSON b, Show b) => [String] -> IO (Either Error b)
githubGet = githubGet' Nothing

githubGet' :: (FromJSON b, Show b) => Maybe BasicAuth -> [String] -> IO (Either Error b)
githubGet' auth paths =
  githubAPI (BS.pack "GET")
            (buildUrl paths)
            auth
            (Nothing :: Maybe Value)

githubGetWithQueryString :: (FromJSON b, Show b) => [String] -> String -> IO (Either Error b)
githubGetWithQueryString = githubGetWithQueryString' Nothing

githubGetWithQueryString' :: (FromJSON b, Show b) => Maybe BasicAuth -> [String] -> String -> IO (Either Error b)
githubGetWithQueryString' auth paths queryString =
  githubAPI (BS.pack "GET")
            (buildUrl paths ++ "?" ++ queryString)
            auth
            (Nothing :: Maybe Value)

githubPost :: (ToJSON a, Show a, FromJSON b, Show b) => BasicAuth -> [String] -> a -> IO (Either Error b)
githubPost auth paths body =
  githubAPI (BS.pack "POST")
            (buildUrl paths)
            (Just auth)
            (Just body)

githubPatch :: (ToJSON a, Show a, FromJSON b, Show b) => BasicAuth -> [String] -> a -> IO (Either Error b)
githubPatch auth paths body =
  githubAPI (BS.pack "PATCH")
            (buildUrl paths)
            (Just auth)
            (Just body)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => BS.ByteString -> String -> Maybe BasicAuth -> Maybe a -> IO (Either Error b)
githubAPI method url auth body = do
  result <- doHttps method url auth (Just encodedBody)
  return $ either (Left . HTTPConnectionError)
                  (parseJson . responseBody)
                  result
  where encodedBody = RequestBodyLBS $ encode $ toJSON body

-- | user/password for HTTP basic access authentication
type BasicAuth = (BS.ByteString, BS.ByteString)

doHttps :: Method -> String -> Maybe BasicAuth -> Maybe (RequestBody (ResourceT IO)) -> IO (Either E.SomeException (Response LBS.ByteString))
doHttps method url auth body = do
  let requestBody = fromMaybe (RequestBodyBS $ BS.pack "") body
      (Just uri) = parseUrl url
      request = uri { method = method
                    , secure = True
                    , port = 443
                    , requestBody = requestBody
                    , checkStatus = successOrMissing
                    }
      authRequest = maybe id (uncurry applyBasicAuth) auth request

  (getResponse authRequest >>= return . Right) `E.catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      E.Handler (\e -> E.throw (e :: E.AsyncException)),
  
      E.Handler (\e -> (return . Left) (e :: E.SomeException))
      ]
  where
    getResponse request = withManager $ \manager -> httpLbs request manager
    successOrMissing s@(Status sci _) hs
      | (200 <= sci && sci < 300) || sci == 404 = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs

parseJson :: (FromJSON b, Show b) => LBS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.ByteString.Lazy.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError $ e ++ " on the JSON: " ++ LBS.unpack jsonString
       (Fail _ _ e) -> Left $ ParseError e
