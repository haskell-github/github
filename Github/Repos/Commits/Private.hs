{-# LANGUAGE OverloadedStrings #-}
module Github.Repos.Commits.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import Network.Curl.Download
import Network.HTTP
import Network.URI

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

fullGithubGet paths = do
  commitsJsonString <- openURI $ buildUrl paths
  return $ either (Left . OpenURIError) parseJson commitsJsonString

fullGithubPost :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
fullGithubPost paths body = do
  let (Just uri) = parseURI $ buildUrl paths
      request = Request {
         rqURI = uri
        ,rqMethod = POST
        ,rqHeaders = []
        ,rqBody = show $ toJSON body
    }
  result <- simpleHTTP request
  return $ either (Left . HTTPConnectionError)
                  (parseJson . BS.pack . rspBody)
                  result

fullGithubPatch :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
fullGithubPatch paths body = do
  let (Just uri) = parseURI $ buildUrl paths
      request = Request {
         rqURI = uri
        ,rqMethod = Custom "PATCH"
        ,rqHeaders = []
        ,rqBody = show $ toJSON body
    }
  result <- simpleHTTP request
  return $ either (Left . HTTPConnectionError)
                  (parseJson . BS.pack . rspBody)
                  result

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError e
       (Fail _ _ e) -> Left $ ParseError e
