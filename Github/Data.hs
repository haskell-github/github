{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Github.Data (module Github.Data.Definitions) where

import Data.Time
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Aeson.Types
import System.Locale (defaultTimeLocale)
import Data.Attoparsec.Number (Number(..))

import Github.Data.Definitions

instance FromJSON GithubDate where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%FT%T%Z" (T.unpack t) of
         Just d -> pure $ GithubDate d
         _      -> fail "could not parse Github datetime"
  parseJSON _          = fail "Given something besides a String"

instance FromJSON Commit where
  parseJSON (Object o) =
    Commit <$> o .: "sha"
           <*> o .: "parents"
           <*> o .: "url"
           <*> o .: "commit"
           <*> o .:? "committer"
           <*> o .:? "author"
           <*> o .:< "files"
           <*> o .:? "stats"
  parseJSON _          = fail "Could not build a Commit"

instance FromJSON Tree where
  parseJSON (Object o) =
    Tree <$> o .: "sha" <*> o .: "url"
  parseJSON _          = fail "Could not build a Tree"

instance FromJSON GitCommit where
  parseJSON (Object o) =
    GitCommit <$> o .: "message"
              <*> o .: "url"
              <*> o .: "committer"
              <*> o .: "author"
              <*> o .: "tree"
  parseJSON _          = fail "Could not build a GitCommit"

instance FromJSON GithubUser where
  parseJSON (Object o) =
    GithubUser <$> o .: "avatar_url"
               <*> o .: "login"
               <*> o .: "url"
               <*> o .: "id"
               <*> o .: "gravatar_id"
  parseJSON v          = fail $ "Could not build a GithubUser out of " ++ (show v)

instance FromJSON GitUser where
  parseJSON (Object o) =
    GitUser <$> o .: "name"
            <*> o .: "email"
            <*> o .: "date"
  parseJSON _          = fail "Could not build a GitUser"

instance FromJSON File where
  parseJSON (Object o) =
    File <$> o .: "blob_url"
         <*> o .: "status"
         <*> o .: "raw_url"
         <*> o .: "additions"
         <*> o .: "sha"
         <*> o .: "changes"
         <*> o .: "patch"
         <*> o .: "filename"
         <*> o .: "deletions"
  parseJSON _          = fail "Could not build a File"

instance FromJSON Stats where
  parseJSON (Object o) =
    Stats <$> o .: "additions"
          <*> o .: "total"
          <*> o .: "deletions"
  parseJSON _          = fail "Could not build a Stats"

instance FromJSON Comment where
  parseJSON (Object o) =
    Comment <$> o .:? "position"
            <*> o .:? "line"
            <*> o .: "body"
            <*> o .: "commit_id"
            <*> o .: "updated_at"
            <*> o .: "html_url"
            <*> o .: "url"
            <*> o .: "created_at"
            <*> o .: "path"
            <*> o .: "user"
            <*> o .: "id"
  parseJSON _          = fail "Could not build a Comment"

instance ToJSON NewComment where
  toJSON newComment =
    Object $ Map.fromList
      [("body", String $ T.pack $ newCommentBody newComment)
      ,("line_number", Number $ Data.Attoparsec.Number.I $ fromIntegral $ newCommentLineNumber newComment)
      ,("path", String $ T.pack $ newCommentPath newComment)]

instance ToJSON UpdatedComment where
  toJSON updatedComment =
    Object $ Map.fromList
      [("body", String $ T.pack $ updatedCommentBody updatedComment)]

(.:<) :: (FromJSON a) => Object -> T.Text -> Parser [a]
obj .:< key = case Map.lookup key obj of
                   Nothing -> pure []
                   Just v  -> parseJSON v
