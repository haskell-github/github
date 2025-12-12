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
  deriving (Show, Eq, Ord, Generic, Data)

data Stats = Stats
    { statsAdditions :: !Int
    , statsTotal     :: !Int
    , statsDeletions :: !Int
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Stats
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
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Commit
instance Binary Commit

data Tree = Tree
    { treeSha      :: !(Name Tree)
    , treeUrl      :: !URL
    , treeGitTrees :: !(Vector GitTree)
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Tree
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
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData GitTree
instance Binary GitTree

data GitCommit = GitCommit
    { gitCommitMessage   :: !Text
    , gitCommitUrl       :: !URL
    , gitCommitCommitter :: !GitUser
    , gitCommitAuthor    :: !GitUser
    , gitCommitTree      :: !Tree
    , gitCommitSha       :: !(Maybe (Name GitCommit))
    , gitCommitParents   :: !(Vector Tree)
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData GitCommit
instance Binary GitCommit

data Blob = Blob
    { blobUrl      :: !URL
    , blobEncoding :: !Text
    , blobContent  :: !Text
    , blobSha      :: !(Name Blob)
    , blobSize     :: !Int
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Blob
instance Binary Blob

data Tag = Tag
    { tagName       :: !Text
    , tagZipballUrl :: !URL
    , tagTarballUrl :: !URL
    , tagCommit     :: !BranchCommit
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Tag
instance Binary Tag

data Branch = Branch
    { branchName   :: !Text
    , branchCommit :: !BranchCommit
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Branch

data BranchCommit = BranchCommit
    { branchCommitSha :: !Text
    , branchCommitUrl :: !URL
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData BranchCommit
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
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData Diff
instance Binary Diff

data NewGitReference = NewGitReference
    { newGitReferenceRef :: !Text
    , newGitReferenceSha :: !Text
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData NewGitReference
instance Binary NewGitReference

data GitReference = GitReference
    { gitReferenceObject :: !GitObject
    , gitReferenceUrl    :: !URL
    , gitReferenceRef    :: !(Name GitReference)
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData GitReference
instance Binary GitReference

data GitObject = GitObject
    { gitObjectType :: !Text
    , gitObjectSha  :: !Text
    , gitObjectUrl  :: !URL
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData GitObject
instance Binary GitObject

data GitUser = GitUser
    { gitUserName  :: !Text
    , gitUserEmail :: !Text
    , gitUserDate  :: !UTCTime
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData GitUser
instance Binary GitUser

data File = File
    { fileBlobUrl   :: !(Maybe URL)
    , fileStatus    :: !Text
    , fileRawUrl    :: !(Maybe URL)
    , fileAdditions :: !Int
    , fileSha       :: !(Maybe Text)
    , fileChanges   :: !Int
    , filePatch     :: !(Maybe Text)
    , fileFilename  :: !Text
    , fileDeletions :: !Int
    }
  deriving (Show, Data, Eq, Ord, Generic)

instance NFData File
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
        <*> (o .:? "committer" <|> pure Nothing)
        <*> (o .:? "author" <|> pure Nothing)
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
        <*> o .:? "sha"
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
