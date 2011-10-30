{-# LANGUAGE OverloadedStrings #-}
module Github.Repos.Commits.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import Network.Curl.Download

parseCommitsJson :: BS.ByteString -> Either String [Commit]
parseCommitsJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              anythingElse ->  Left $ show anythingElse
       undone -> Left $ show undone

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubApiGet :: String -> IO (Either String BS.ByteString)
githubApiGet = openURI
