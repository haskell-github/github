{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Github.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as Types
import Network.HTTP.Enumerator
import Text.URI
import Control.Failure hiding (Error(..))
import qualified Control.Exception as E

githubGet :: (FromJSON b, Show b) => [String] -> IO (Either Error b)
githubGet paths =
  githubAPI (BS.pack "GET")
            (buildUrl paths)
            (Nothing :: Maybe Value)

githubGetWithQueryString :: (FromJSON b, Show b) => [String] -> String -> IO (Either Error b)
githubGetWithQueryString paths queryString =
  githubAPI (BS.pack "GET")
            (buildUrl paths ++ "?" ++ queryString)
            (Nothing :: Maybe Value)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => BS.ByteString -> String -> Maybe a -> IO (Either Error b)
githubAPI method url body =
  doHttp method url body
         (LBS.unpack . encode . toJSON)
         (parseJson . BS.pack . LBS.unpack . responseBody)

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError $ e ++ " on the JSON: " ++ BS.unpack jsonString
       (Fail _ _ e) -> Left $ ParseError e

doHttp :: (Show a) => BS.ByteString -> String -> Maybe a -> (String -> String) -> (Response -> Either Error b) -> IO (Either Error b)
doHttp method url body bodyConversion successConversion = do
  let (Just uri) = parseURI url
      (Just host) = uriRegName uri
      encodedBody = BS.pack $ maybe "" (bodyConversion . show) body
      queryString = Types.parseQuery $ BS.pack $ maybe "" id $ uriQuery uri
      request = def { method = method
                    , secure = True
                    , host = BS.pack host
                    , port = 443
                    , path = BS.pack $ uriPath uri
                    , requestBody = RequestBodyBS encodedBody
                    , queryString = queryString
                    }

  result <- (getResponse request >>= return . Right) `catch` (return . Left)

  return $ either (Left . HTTPConnectionError)
                  successConversion
                  result
  where
    getResponse request =
      withManager $ \manager -> httpLbs request manager
