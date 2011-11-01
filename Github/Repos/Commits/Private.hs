{-# LANGUAGE OverloadedStrings #-}
module Github.Repos.Commits.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.List
import qualified Data.ByteString.Char8 as BS
import Network.HTTP
import Network.URI

githubGet :: (FromJSON b, Show b) => [String] -> IO (Either Error b)
githubGet paths = githubAPI GET paths (Nothing :: Maybe Value)

githubPost :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
githubPost paths body = githubAPI POST paths (Just body)

githubPatch :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
githubPatch paths body = githubAPI (Custom "PATCH") paths (Just body)

githubDelete :: (FromJSON b, Show b) => [String] -> IO (Either Error b)
githubDelete paths = githubAPI DELETE paths (Nothing :: Maybe Value)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => RequestMethod -> [String] -> Maybe a -> IO (Either Error b)
githubAPI method paths body = do
  let (Just uri) = parseURI $ buildUrl paths
      request = Request {
         rqURI = uri
        ,rqMethod = method
        ,rqHeaders = []
        ,rqBody = show $ maybe "" toJSON body
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
