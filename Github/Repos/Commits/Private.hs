{-# LANGUAGE OverloadedStrings #-}
module Github.Repos.Commits.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import Network.Curl.Download

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

fullGithubGet paths = do
  commitsJsonString <- openURI $ buildUrl paths
  return $ either Left parseJson commitsJsonString

githubApiGet :: String -> IO (Either String BS.ByteString)
githubApiGet = openURI

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either String b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              anythingElse -> Left $ show anythingElse
       undone -> Left $ show undone
