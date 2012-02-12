{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Github.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec.ByteString.Lazy
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as Types
import Network.HTTP.Conduit
import Text.URI
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Resource (runResourceT, withIO)

import Github.Data.Definitions (GithubConfig(..))

githubGet :: (FromJSON b, Show b) => GithubConfig -> [String] -> IO (Either Error b)
githubGet c paths = do
  githubAPI (getHttpManager c)
            (BS.pack "GET")
            (buildUrl paths)
            (Nothing :: Maybe Value)

githubGetWithQueryString :: (FromJSON b, Show b) => GithubConfig -> [String] -> String -> IO (Either Error b)
githubGetWithQueryString c paths queryString =
  githubAPI (getHttpManager c)
            (BS.pack "GET")
            (buildUrl paths ++ "?" ++ queryString)
            (Nothing :: Maybe Value)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => IO Manager -> BS.ByteString -> String -> Maybe a -> IO (Either Error b)
githubAPI m method url body = do
  result <- doHttps m method url (Just encodedBody)
  return $ either (Left . HTTPConnectionError)
                  (parseJson . responseBody)
                  result
  where encodedBody = RequestBodyLBS $ encode $ toJSON body

doHttps :: IO Manager -> BS.ByteString -> String -> Maybe (RequestBody IO) -> IO (Either E.IOException (Response LBS.ByteString))
doHttps m method url body = do
  let (Just uri) = parseURI url
      (Just host) = uriRegName uri
      requestBody = fromMaybe (RequestBodyBS $ BS.pack "") body
      queryString = BS.pack $ fromMaybe "" $ uriQuery uri
      request = def { method = method
                    , secure = True
                    , host = BS.pack host
                    , port = 443
                    , path = BS.pack $ uriPath uri
                    , requestBody = requestBody
                    , queryString = queryString
                    }

  (getResponse request >>= return . Right) `catch` (return . Left)
  where
    getResponse request = runResourceT $ do
      (_, manager) <- withIO m closeManager
      httpLbs request manager
    
    

parseJson :: (FromJSON b, Show b) => LBS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.ByteString.Lazy.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError $ e ++ " on the JSON: " ++ LBS.unpack jsonString
       (Fail _ _ e) -> Left $ ParseError e
