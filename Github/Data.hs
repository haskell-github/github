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
import qualified Data.Vector as V

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
    Tree <$> o .: "sha"
         <*> o .: "url"
         <*> o .:< "tree"
  parseJSON _          = fail "Could not build a Tree"

instance FromJSON GitTree where
  parseJSON (Object o) =
    GitTree <$> o .: "type"
         <*> o .: "sha"
         <*> o .: "url"
         <*> o .:? "size"
         <*> o .: "path"
         <*> o .: "mode"
  parseJSON _          = fail "Could not build a GitTree"

instance FromJSON GitCommit where
  parseJSON (Object o) =
    GitCommit <$> o .: "message"
              <*> o .: "url"
              <*> o .: "committer"
              <*> o .: "author"
              <*> o .: "tree"
              <*> o .:? "sha"
              <*> o .:< "parents"
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

instance FromJSON Diff where
  parseJSON (Object o) =
    Diff <$> o .: "status"
         <*> o .: "behind_by"
         <*> o .: "patch_url"
         <*> o .: "url"
         <*> o .: "base_commit"
         <*> o .:< "commits"
         <*> o .: "total_commits"
         <*> o .: "html_url"
         <*> o .:< "files"
         <*> o .: "ahead_by"
         <*> o .: "diff_url"
         <*> o .: "permalink_url"
  parseJSON _          = fail "Could not build a Diff"

instance FromJSON Gist where
  parseJSON (Object o) =
    Gist <$> o .: "user"
         <*> o .: "git_push_url"
         <*> o .: "url"
         <*> o .:? "description"
         <*> o .: "created_at"
         <*> o .: "public"
         <*> o .: "comments"
         <*> o .: "updated_at"
         <*> o .: "html_url"
         <*> o .: "id"
         <*> o `values` "files"
         <*> o .: "git_push_url"
  parseJSON _          = fail "Could not build a Gist"

instance FromJSON GistFile where
  parseJSON (Object o) =
    GistFile <$> o .: "type"
             <*> o .: "raw_url"
             <*> o .: "size"
             <*> o .:? "language"
             <*> o .: "filename"
             <*> o .:? "content"
  parseJSON _          = fail "Could not build a GistFile"

instance FromJSON GistComment where
  parseJSON (Object o) =
    GistComment <$> o .: "user"
                <*> o .: "url"
                <*> o .: "created_at"
                <*> o .: "body"
                <*> o .: "updated_at"
                <*> o .: "id"
  parseJSON _          = fail "Could not build a GistComment"

instance FromJSON Blob where
  parseJSON (Object o) =
    Blob <$> o .: "url"
         <*> o .: "encoding"
         <*> o .: "content"
         <*> o .: "sha"
         <*> o .: "size"
  parseJSON _          = fail "Could not build a Blob"

instance FromJSON GitReference where
  parseJSON (Object o) =
    GitReference <$> o .: "object"
                 <*> o .: "url"
                 <*> o .: "ref"
  parseJSON _          = fail "Could not build a GitReference"

instance FromJSON GitObject where
  parseJSON (Object o) =
    GitObject <$> o .: "type"
           <*> o .: "sha"
           <*> o .: "url"
  parseJSON _          = fail "Could not build a GitObject"

-- | A better version of Aeson's .:?, using `mzero' instead of `Nothing'
(.:<) :: (FromJSON a) => Object -> T.Text -> Parser [a]
obj .:< key = case Map.lookup key obj of
                   Nothing -> pure mzero
                   Just v  -> parseJSON v

obj `values` key =
  let (Object children) = Map.findWithDefault (Object Map.empty) key obj in
    parseJSON $ Array $ V.fromList $ Map.elems children
