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
            <*> o .:? "html_url"
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

instance FromJSON Issue where
  parseJSON (Object o) =
    Issue <$> o .:? "closed_at"
          <*> o .: "updated_at"
          <*> o .: "html_url"
          <*> o .:? "closed_by"
          <*> o .: "labels"
          <*> o .: "number"
          <*> o .:? "assignee"
          <*> o .: "user"
          <*> o .: "title"
          <*> o .: "pull_request"
          <*> o .: "url"
          <*> o .: "created_at"
          <*> o .: "body"
          <*> o .: "state"
          <*> o .: "id"
          <*> o .: "comments"
          <*> o .:? "milestone"
  parseJSON _          = fail "Could not build an Issue"

instance FromJSON Milestone where
  parseJSON (Object o) =
    Milestone <$> o .: "creator"
              <*> o .: "due_on"
              <*> o .: "open_issues"
              <*> o .: "number"
              <*> o .: "closed_issues"
              <*> o .: "description"
              <*> o .: "title"
              <*> o .: "url"
              <*> o .: "created_at"
              <*> o .: "state"
  parseJSON _          = fail "Could not build a Milestone"

instance FromJSON IssueLabel where
  parseJSON (Object o) =
    IssueLabel <$> o .: "color"
               <*> o .: "url"
               <*> o .: "name"
  parseJSON _          = fail "Could not build a Milestone"

instance FromJSON PullRequestReference where
  parseJSON (Object o) =
    PullRequestReference <$> o .:? "html_url"
                         <*> o .:? "patch_url"
                         <*> o .:? "diff_url"
  parseJSON _          = fail "Could not build a PullRequest"

instance FromJSON IssueComment where
  parseJSON (Object o) =
    IssueComment <$> o .: "updated_at"
                 <*> o .: "user"
                 <*> o .: "url"
                 <*> o .: "created_at"
                 <*> o .: "body"
                 <*> o .: "id"
  parseJSON _          = fail "Could not build an IssueComment"

instance FromJSON Event where
  parseJSON (Object o) =
    Event <$> o .: "actor"
          <*> o .: "event"
          <*> o .:? "commit_id"
          <*> o .: "url"
          <*> o .: "created_at"
          <*> o .: "id"
          <*> o .:? "issue"
  parseJSON _          = fail "Could not build an Event"

instance FromJSON EventType where
  parseJSON (String "closed") = pure Closed
  parseJSON (String "reopened") = pure Reopened
  parseJSON (String "subscribed") = pure Subscribed
  parseJSON (String "merged") = pure Merged
  parseJSON (String "referenced") = pure Referenced
  parseJSON (String "mentioned") = pure Mentioned
  parseJSON (String "assigned") = pure Assigned
  parseJSON (String "unsubscribed") = pure Unsubscribed
  parseJSON _ = fail "Could not build an EventType"

instance FromJSON SimpleOrganization where
  parseJSON (Object o) =
    SimpleOrganization <$> o .: "url"
                       <*> o .: "avatar_url"
                       <*> o .: "id"
                       <*> o .: "login"
  parseJSON _ = fail "Could not build a SimpleOrganization"

instance FromJSON Organization where
  parseJSON (Object o) =
    Organization <$> o .: "type"
                 <*> o .:? "blog"
                 <*> o .:? "location"
                 <*> o .: "login"
                 <*> o .: "followers"
                 <*> o .:? "company"
                 <*> o .: "avatar_url"
                 <*> o .: "public_gists"
                 <*> o .: "html_url"
                 <*> o .:? "email"
                 <*> o .: "following"
                 <*> o .: "public_repos"
                 <*> o .: "url"
                 <*> o .: "created_at"
                 <*> o .:? "name"
                 <*> o .: "id"
  parseJSON _ = fail "Could not build an Organization"

instance FromJSON PullRequest where
  parseJSON (Object o) =
      PullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
  parseJSON _ = fail "Could not build a PullRequest"

instance FromJSON DetailedPullRequest where
  parseJSON (Object o) =
      DetailedPullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
        <*> o .:? "merged_by"
        <*> o .: "changed_files"
        <*> o .: "head"
        <*> o .: "comments"
        <*> o .: "deletions"
        <*> o .: "additions"
        <*> o .: "review_comments"
        <*> o .: "base"
        <*> o .: "commits"
        <*> o .: "merged"
        <*> o .: "mergeable"
  parseJSON _ = fail "Could not build a DetailedPullRequest"

instance FromJSON PullRequestLinks where
  parseJSON (Object o) =
    PullRequestLinks <$> o <.:> ["review_comments", "href"]
                     <*> o <.:> ["comments", "href"]
                     <*> o <.:> ["html", "href"]
                     <*> o <.:> ["self", "href"]
  parseJSON _ = fail "Could not build a PullRequestLinks"

instance FromJSON PullRequestCommit where
  parseJSON (Object o) =
    return PullRequestCommit
  parseJSON _ = fail "Could not build a PullRequestCommit"

instance FromJSON Repo where
  parseJSON (Object o) =
    Repo <$> o .: "ssh_url"
         <*> o .: "description"
         <*> o .: "created_at"
         <*> o .: "html_url"
         <*> o .: "svn_url"
         <*> o .: "forks"
         <*> o .:? "homepage"
         <*> o .: "fork"
         <*> o .: "git_url"
         <*> o .: "private"
         <*> o .: "clone_url"
         <*> o .: "size"
         <*> o .: "updated_at"
         <*> o .: "watchers"
         <*> o .: "owner"
         <*> o .: "name"
         <*> o .: "language"
         <*> o .:? "master_branch"
         <*> o .: "pushed_at"
         <*> o .: "id"
         <*> o .: "url"
         <*> o .: "open_issues"
  parseJSON _ = fail "Could not build a Repo"


-- | A better version of Aeson's .:?, using `mzero' instead of `Nothing'.
(.:<) :: (FromJSON a) => Object -> T.Text -> Parser [a]
obj .:< key = case Map.lookup key obj of
                   Nothing -> pure mzero
                   Just v  -> parseJSON v

-- | Produce all values for the given key.
obj `values` key =
  let (Object children) = Map.findWithDefault (Object Map.empty) key obj in
    parseJSON $ Array $ V.fromList $ Map.elems children

-- | Produce the value for the last key, traversing.
obj <.:> [key] = obj .: key
obj <.:> (key:keys) =
  let (Object nextObj) = Map.findWithDefault (Object Map.empty) key obj in
      nextObj <.:> keys
