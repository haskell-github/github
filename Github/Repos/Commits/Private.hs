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
  return $ either Left parseJson commitsJsonString

-- fullGithubPost :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either String b)
fullGithubPost paths body = do
  let (Just uri) = parseURI $ buildUrl paths
      request = Request {
         rqURI = uri
        ,rqMethod = POST
        ,rqHeaders = []
        ,rqBody = show $ toJSON body
    }
  result <- simpleHTTP request
  return $ either (Left . show) (parseJson . BS.pack . rspBody) result

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either String b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              anythingElse -> Left $ show anythingElse
       undone -> Left $ show undone
