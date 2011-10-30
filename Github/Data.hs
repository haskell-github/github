{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Github.Data (
  Commit(..)
, Author(..)
, GithubDate(..)
) where

import Data.Time
import Data.Typeable
import Data.Data
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Aeson.Types
import System.Locale (defaultTimeLocale)

newtype GithubDate = GithubDate { fromGithubDate :: UTCTime }
  deriving (Show, Data, Typeable)

data Commit = Commit {
   commitSha     :: String
  ,commitAuthor  :: Author
  ,commitMessage :: String
} deriving (Show, Data, Typeable)

data Author = Author {
   authorName  :: String
  ,authorEmail :: String
  ,authorDate  :: GithubDate
} deriving (Show, Data, Typeable)

instance FromJSON Commit where
  parseJSON (Object o) =
    Commit <$> o .: "sha"
           <*> o .:/ ["commit","author"]
           <*> o .:/ ["commit", "message"]
  parseJSON _          = mzero

instance FromJSON Author where
  parseJSON (Object o) =
    Author <$> o .: "name"
           <*> o .: "email"
           <*> o .: "date"
  parseJSON _          = mzero

instance FromJSON GithubDate where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%FT%T%Z" (T.unpack t) of
         Just d -> pure $ GithubDate d
         _      -> fail "could not parse Github datetime"
  parseJSON v   = mzero

(.:/) :: (FromJSON a) => Object -> [T.Text] -> Parser a
o .:/ [] = fail "could not find the unknown key in the JSON"
o .:/ xs = l (Object o) xs
  where
    l result [] = parseJSON result
    l (Object o') (key:keys) =
      case Map.lookup key o' of
        Nothing -> fail $ "could not find " ++ (show xs) ++ " in the JSON"
        (Just v) -> l v keys




-- obj .: key = case M.lookup key obj of
--                Nothing -> fail $ "key " ++ show key ++ " not present"
--                Just v  -> parseJSON v
