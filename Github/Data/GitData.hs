{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.GitData where

import Github.Data.Definitions
import Github.Data.Name        (Name)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

-- | The options for querying commits.
data CommitQueryOption = CommitQuerySha !Text
                       | CommitQueryPath !Text
                       | CommitQueryAuthor !Text
                       | CommitQuerySince !UTCTime
                       | CommitQueryUntil !UTCTime
                       deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Stats = Stats {
   statsAdditions :: !Int
  ,statsTotal     :: !Int
  ,statsDeletions :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats where rnf = genericRnf
instance Binary Stats

data Commit = Commit {
   commitSha       :: !(Name Commit)
  ,commitParents   :: !(Vector Tree)
  ,commitUrl       :: !Text
  ,commitGitCommit :: !GitCommit
  ,commitCommitter :: !(Maybe SimpleUser)
  ,commitAuthor    :: !(Maybe SimpleUser)
  ,commitFiles     :: !(Vector File)
  ,commitStats     :: !(Maybe Stats)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Commit where rnf = genericRnf
instance Binary Commit

data Tree = Tree {
   treeSha      :: !(Name Tree)
  ,treeUrl      :: !Text
  ,treeGitTrees :: !(Vector GitTree)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tree where rnf = genericRnf
instance Binary Tree

data GitTree = GitTree {
  gitTreeType  :: !Text
  ,gitTreeSha  :: !(Name GitTree)
  -- Can be empty for submodule
  ,gitTreeUrl  :: !(Maybe Text)
  ,gitTreeSize :: !(Maybe Int)
  ,gitTreePath :: !Text
  ,gitTreeMode :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitTree where rnf = genericRnf
instance Binary GitTree

data GitCommit = GitCommit {
   gitCommitMessage   :: !Text
  ,gitCommitUrl       :: !Text
  ,gitCommitCommitter :: !GitUser
  ,gitCommitAuthor    :: !GitUser
  ,gitCommitTree      :: !Tree
  ,gitCommitSha       :: !(Maybe (Name GitCommit))
  ,gitCommitParents   :: !(Vector Tree)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitCommit where rnf = genericRnf
instance Binary GitCommit

data Blob = Blob {
   blobUrl      :: !Text
  ,blobEncoding :: !Text
  ,blobContent  :: !Text
  ,blobSha      :: !(Name Blob)
  ,blobSize     :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Blob where rnf = genericRnf
instance Binary Blob

data Tag = Tag {
   tagName       :: !Text
  ,tagZipballUrl :: !Text
  ,tagTarballUrl :: !Text
  ,tagCommit     :: !BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tag where rnf = genericRnf
instance Binary Tag

data Branch = Branch {
   branchName   :: !Text
  ,branchCommit :: !BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Branch where rnf = genericRnf

data BranchCommit = BranchCommit {
   branchCommitSha :: !Text
  ,branchCommitUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData BranchCommit where rnf = genericRnf
instance Binary BranchCommit

data Diff = Diff {
   diffStatus       :: !Text
  ,diffBehindBy     :: !Int
  ,diffPatchUrl     :: !Text
  ,diffUrl          :: !Text
  ,diffBaseCommit   :: !Commit
  ,diffCommits      :: !(Vector Commit)
  ,diffTotalCommits :: !Int
  ,diffHtmlUrl      :: !Text
  ,diffFiles        :: !(Vector File)
  ,diffAheadBy      :: !Int
  ,diffDiffUrl      :: !Text
  ,diffPermalinkUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Diff where rnf = genericRnf
instance Binary Diff

data NewGitReference = NewGitReference {
   newGitReferenceRef :: !Text
  ,newGitReferenceSha :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewGitReference where rnf = genericRnf
instance Binary NewGitReference

data GitReference = GitReference {
   gitReferenceObject :: !GitObject
  ,gitReferenceUrl    :: !Text
  ,gitReferenceRef    :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitReference where rnf = genericRnf
instance Binary GitReference

data GitObject = GitObject {
   gitObjectType :: !Text
  ,gitObjectSha  :: !Text
  ,gitObjectUrl  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitObject where rnf = genericRnf
instance Binary GitObject

data GitUser = GitUser {
   gitUserName  :: !Text
  ,gitUserEmail :: !Text
  ,gitUserDate  :: !UTCTime
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitUser where rnf = genericRnf
instance Binary GitUser

data File = File {
   fileBlobUrl   :: !Text
  ,fileStatus    :: !Text
  ,fileRawUrl    :: !Text
  ,fileAdditions :: !Int
  ,fileSha       :: !Text
  ,fileChanges   :: !Int
  ,filePatch     :: !(Maybe Text)
  ,fileFilename  :: !Text
  ,fileDeletions :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData File where rnf = genericRnf
instance Binary File
