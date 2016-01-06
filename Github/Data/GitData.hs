{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.GitData where

import Github.Data.Definitions

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import GHC.Generics    (Generic)

data Commit = Commit {
   commitSha       :: String
  ,commitParents   :: [Tree]
  ,commitUrl       :: String
  ,commitGitCommit :: GitCommit
  ,commitCommitter :: Maybe GithubOwner
  ,commitAuthor    :: Maybe GithubOwner
  ,commitFiles     :: [File]
  ,commitStats     :: Maybe Stats
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Commit

data Tree = Tree {
   treeSha      :: String
  ,treeUrl      :: String
  ,treeGitTrees :: [GitTree]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tree

data GitTree = GitTree {
  gitTreeType  :: String
  ,gitTreeSha  :: String
  -- Can be empty for submodule
  ,gitTreeUrl  :: Maybe String
  ,gitTreeSize :: Maybe Int
  ,gitTreePath :: String
  ,gitTreeMode :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitTree

data GitCommit = GitCommit {
   gitCommitMessage   :: String
  ,gitCommitUrl       :: String
  ,gitCommitCommitter :: GitUser
  ,gitCommitAuthor    :: GitUser
  ,gitCommitTree      :: Tree
  ,gitCommitSha       :: Maybe String
  ,gitCommitParents   :: [Tree]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitCommit

data Blob = Blob {
   blobUrl      :: String
  ,blobEncoding :: String
  ,blobContent  :: String
  ,blobSha      :: String
  ,blobSize     :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Blob

data Tag = Tag {
   tagName       :: String
  ,tagZipballUrl :: String
  ,tagTarballUrl :: String
  ,tagCommit     :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tag

data Branch = Branch {
   branchName   :: String
  ,branchCommit :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Branch

data BranchCommit = BranchCommit {
   branchCommitSha :: String
  ,branchCommitUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData BranchCommit

data Diff = Diff {
   diffStatus       :: String
  ,diffBehindBy     :: Int
  ,diffPatchUrl     :: String
  ,diffUrl          :: String
  ,diffBaseCommit   :: Commit
  ,diffCommits      :: [Commit]
  ,diffTotalCommits :: Int
  ,diffHtmlUrl      :: String
  ,diffFiles        :: [File]
  ,diffAheadBy      :: Int
  ,diffDiffUrl      :: String
  ,diffPermalinkUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Diff

data NewGitReference = NewGitReference {
   newGitReferenceRef :: String
  ,newGitReferenceSha :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewGitReference

data GitReference = GitReference {
   gitReferenceObject :: GitObject
  ,gitReferenceUrl    :: String
  ,gitReferenceRef    :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitReference

data GitObject = GitObject {
   gitObjectType :: String
  ,gitObjectSha  :: String
  ,gitObjectUrl  :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitObject

data GitUser = GitUser {
   gitUserName  :: String
  ,gitUserEmail :: String
  ,gitUserDate  :: GithubDate
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitUser

data File = File {
   fileBlobUrl   :: String
  ,fileStatus    :: String
  ,fileRawUrl    :: String
  ,fileAdditions :: Int
  ,fileSha       :: String
  ,fileChanges   :: Int
  ,filePatch     :: String
  ,fileFilename  :: String
  ,fileDeletions :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData File
