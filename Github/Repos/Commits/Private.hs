{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Github.Repos.Commits.Private where

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
githubGet paths = githubAPI (BS.pack "GET") paths (Nothing :: Maybe Value)

githubPost :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
githubPost paths body = githubAPI (BS.pack "POST") paths (Just body)

githubPatch :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> IO (Either Error b)
githubPatch paths body = githubAPI (BS.pack "PATCH") paths (Just body)

githubDelete :: (FromJSON b, Show b) => [String] -> IO (Either Error b)
githubDelete paths = githubAPI (BS.pack "DELETE") paths (Nothing :: Maybe Value)

buildUrl :: [String] -> String
buildUrl paths = "https://api.github.com/" ++ intercalate "/" paths

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => BS.ByteString -> [String] -> Maybe a -> IO (Either Error b)
githubAPI method paths body = do
  let (Just uri) = parseURI $ buildUrl paths
      (Just host) = uriRegName uri
      encodedBody = RequestBodyBS $ BS.pack $ maybe "" (show . toJSON) body
      request = def { method = method
                    , secure = True
                    , host = BS.pack host
                    , port = 443
                    , path = BS.pack $ uriPath uri
                    , requestBody = encodedBody
                    }

  result <- (getResponse request >>= return . Right) `catch` (return . Left)
  return $ either (Left . HTTPConnectionError)
                  (parseJson . BS.pack . LBS.unpack . responseBody)
                  result
  where
    getResponse request =
      withManager $ \manager -> httpLbs request manager

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError e
       (Fail _ _ e) -> Left $ ParseError e
