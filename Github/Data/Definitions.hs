{-# LANGUAGE DeriveDataTypeable #-}

module Github.Data.Definitions (
  GithubDate(..)
, Commit(..)
, Tree(..)
, GitCommit(..)
, GithubUser(..)
, GitUser(..)
) where

import Data.Time
import Data.Data

newtype GithubDate = GithubDate { fromGithubDate :: UTCTime }
  deriving (Show, Data, Typeable)

data Commit = Commit {
   commitSha       :: String
  ,commitParents   :: [Tree]
  ,commitUrl       :: String
  ,commitGitCommit :: GitCommit
  ,commitCommitter :: Maybe GithubUser
  ,commitAuthor    :: Maybe GithubUser
} deriving (Show, Data, Typeable)

data Tree = Tree {
   treeSha :: String
  ,treeUrl :: String
} deriving (Show, Data, Typeable)

data GitCommit = GitCommit {
   gitCommitMessage :: String
  ,gitCommitUrl :: String
  ,gitCommitCommitter :: GitUser
  ,gitCommitAuthor :: GitUser
  ,gitCommitTree :: Tree
} deriving (Show, Data, Typeable)

data GithubUser = GithubUser {
   githubUserAvatarUrl :: String
  ,githubUserLogin :: String
  ,githubUserUrl :: String
  ,githubUserId :: Int
  ,githubUserGravatarId :: String
} deriving (Show, Data, Typeable)

data GitUser = GitUser {
   gitUserName  :: String
  ,gitUserEmail :: String
  ,gitUserDate  :: GithubDate
} deriving (Show, Data, Typeable)
