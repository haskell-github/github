-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.GitData where

import GitHub.Data.Definitions
import GitHub.Data.Name        (Name)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Vector as V

-- | The options for querying commits.
data CommitQueryOption
    = CommitQuerySha !Text
    | CommitQueryPath !Text
    | CommitQueryAuthor !Text
    | CommitQuerySince !UTCTime
    | CommitQueryUntil !UTCTime
  deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Stats = Stats
    { statsAdditions :: !Int
    , statsTotal     :: !Int
    , statsDeletions :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats where rnf = genericRnf
instance Binary Stats

data Commit = Commit
    { commitSha       :: !(Name Commit)
    , commitParents   :: !(Vector Tree)
    , commitUrl       :: !URL
    , commitGitCommit :: !GitCommit
    , commitCommitter :: !(Maybe SimpleUser)
    , commitAuthor    :: !(Maybe SimpleUser)
    , commitFiles     :: !(Vector File)
    , commitStats     :: !(Maybe Stats)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Commit where rnf = genericRnf
instance Binary Commit

data Tree = Tree
    { treeSha      :: !(Name Tree)
    , treeUrl      :: !URL
    , treeGitTrees :: !(Vector GitTree)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tree where rnf = genericRnf
instance Binary Tree

data GitTree = GitTree
    { gitTreeType :: !Text
    , gitTreeSha  :: !(Name GitTree)
    -- Can be empty for submodule
    , gitTreeUrl  :: !(Maybe URL)
    , gitTreeSize :: !(Maybe Int)
    , gitTreePath :: !Text
    , gitTreeMode :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitTree where rnf = genericRnf
instance Binary GitTree

data NewTree = NewTree
    { newBaseTree :: !(Name Tree)
    , newTreeGitTrees :: !(Vector NewGitTree)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewTree where rnf = genericRnf
instance Binary NewTree

data NewGitTree = NewGitTree
    { newGitTreePath :: !Text
    , newGitTreeMode :: !Text
    , newGitTreeContent :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewGitTree where rnf = genericRnf
instance Binary NewGitTree

data GitCommit = GitCommit
    { gitCommitMessage   :: !Text
    , gitCommitUrl       :: !URL
    , gitCommitCommitter :: !GitUser
    , gitCommitAuthor    :: !GitUser
    , gitCommitTree      :: !Tree
    , gitCommitSha       :: !(Maybe (Name GitCommit))
    , gitCommitParents   :: !(Vector Tree)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitCommit where rnf = genericRnf
instance Binary GitCommit

data Blob = Blob
    { blobUrl      :: !URL
    , blobEncoding :: !Text
    , blobContent  :: !Text
    , blobSha      :: !(Name Blob)
    , blobSize     :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Blob where rnf = genericRnf
instance Binary Blob

data Tag = Tag
    { tagName       :: !Text
    , tagZipballUrl :: !URL
    , tagTarballUrl :: !URL
    , tagCommit     :: !BranchCommit
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tag where rnf = genericRnf
instance Binary Tag

data Branch = Branch
    { branchName   :: !Text
    , branchCommit :: !BranchCommit
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Branch where rnf = genericRnf

data BranchCommit = BranchCommit
    { branchCommitSha :: !Text
    , branchCommitUrl :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData BranchCommit where rnf = genericRnf
instance Binary BranchCommit

data Diff = Diff
    { diffStatus       :: !Text
    , diffBehindBy     :: !Int
    , diffPatchUrl     :: !URL
    , diffUrl          :: !URL
    , diffBaseCommit   :: !Commit
    , diffCommits      :: !(Vector Commit)
    , diffTotalCommits :: !Int
    , diffHtmlUrl      :: !URL
    , diffFiles        :: !(Vector File)
    , diffAheadBy      :: !Int
    , diffDiffUrl      :: !URL
    , diffPermalinkUrl :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Diff where rnf = genericRnf
instance Binary Diff

data NewGitReference = NewGitReference
    { newGitReferenceRef :: !Text
    , newGitReferenceSha :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewGitReference where rnf = genericRnf
instance Binary NewGitReference

data GitReference = GitReference
    { gitReferenceObject :: !GitObject
    , gitReferenceUrl    :: !URL
    , gitReferenceRef    :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitReference where rnf = genericRnf
instance Binary GitReference

data GitObject = GitObject
    { gitObjectType :: !Text
    , gitObjectSha  :: !Text
    , gitObjectUrl  :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitObject where rnf = genericRnf
instance Binary GitObject

data GitUser = GitUser
    { gitUserName  :: !Text
    , gitUserEmail :: !Text
    , gitUserDate  :: !UTCTime
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitUser where rnf = genericRnf
instance Binary GitUser

data File = File
    { fileBlobUrl   :: !(Maybe URL)
    , fileStatus    :: !Text
    , fileRawUrl    :: !(Maybe URL)
    , fileAdditions :: !Int
    , fileSha       :: !Text
    , fileChanges   :: !Int
    , filePatch     :: !(Maybe Text)
    , fileFilename  :: !Text
    , fileDeletions :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData File where rnf = genericRnf
instance Binary File

-- JSON instances

instance FromJSON Stats where
    parseJSON = withObject "Stats" $ \o -> Stats
        <$> o .: "additions"
        <*> o .: "total"
        <*> o .: "deletions"

instance FromJSON Commit where
    parseJSON = withObject "Commit" $ \o -> Commit
        <$> o .: "sha"
        <*> o .: "parents"
        <*> o .: "url"
        <*> o .: "commit"
        <*> o .:? "committer"
        <*> o .:? "author"
        <*> o .:? "files" .!= V.empty
        <*> o .:? "stats"

instance FromJSON Tree where
    parseJSON = withObject "Tree" $ \o -> Tree
        <$> o .: "sha"
        <*> o .: "url"
        <*> o .:? "tree" .!= V.empty

instance FromJSON GitTree where
    parseJSON = withObject "GitTree" $ \o -> GitTree
        <$> o .: "type"
        <*> o .: "sha"
        <*> o .:? "url"
        <*> o .:? "size"
        <*> o .: "path"
        <*> o .: "mode"


instance ToJSON NewTree where
    toJSON (NewTree b tree) = object [ "base_tree" .= b, "tree" .= tree  ]

instance ToJSON NewGitTree where
    toJSON (NewGitTree path mode content) = object [ "path" .= path, "mode" .= mode, "content" .= content  ]

instance FromJSON GitCommit where
    parseJSON = withObject "GitCommit" $ \o -> GitCommit
        <$> o .: "message"
        <*> o .: "url"
        <*> o .: "committer"
        <*> o .: "author"
        <*> o .: "tree"
        <*> o .:? "sha"
        <*> o .:? "parents" .!= V.empty

instance FromJSON GitUser where
    parseJSON = withObject "GitUser" $ \o -> GitUser
        <$> o .: "name"
        <*> o .: "email"
        <*> o .: "date"

instance FromJSON File where
    parseJSON = withObject "File" $ \o -> File
        <$> o .:? "blob_url"
        <*> o .: "status"
        <*> o .:? "raw_url"
        <*> o .: "additions"
        <*> o .: "sha"
        <*> o .: "changes"
        <*> o .:? "patch"
        <*> o .: "filename"
        <*> o .: "deletions"

instance ToJSON NewGitReference where
    toJSON (NewGitReference r s) = object [ "ref" .= r, "sha" .= s  ]

instance FromJSON GitReference where
    parseJSON = withObject "GitReference" $ \o -> GitReference
        <$> o .: "object"
        <*> o .: "url"
        <*> o .: "ref"

instance FromJSON GitObject where
    parseJSON = withObject "GitObject" $ \o -> GitObject
        <$> o .: "type"
        <*> o .: "sha"
        <*> o .: "url"

instance FromJSON Diff where
    parseJSON = withObject "Diff" $ \o -> Diff
        <$> o .: "status"
        <*> o .: "behind_by"
        <*> o .: "patch_url"
        <*> o .: "url"
        <*> o .: "base_commit"
        <*> o .:? "commits" .!= V.empty
        <*> o .: "total_commits"
        <*> o .: "html_url"
        <*> o .:? "files" .!= V.empty
        <*> o .: "ahead_by"
        <*> o .: "diff_url"
        <*> o .: "permalink_url"

instance FromJSON Blob where
    parseJSON = withObject "Blob" $ \o -> Blob
        <$> o .: "url"
        <*> o .: "encoding"
        <*> o .: "content"
        <*> o .: "sha"
        <*> o .: "size"

instance FromJSON Tag where
    parseJSON = withObject "Tag" $ \o -> Tag
        <$> o .: "name"
        <*> o .: "zipball_url"
        <*> o .: "tarball_url"
        <*> o .: "commit"

instance FromJSON Branch where
    parseJSON = withObject "Branch" $ \o -> Branch
        <$> o .: "name"
        <*> o .: "commit"

instance FromJSON BranchCommit where
    parseJSON = withObject "BranchCommit" $ \o -> BranchCommit
        <$> o .: "sha"
        <*> o .: "url"
