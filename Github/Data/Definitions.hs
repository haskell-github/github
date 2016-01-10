{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Github.Data.Definitions where

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import Data.Time       (UTCTime)
import Data.Vector     (Vector)
import GHC.Generics    (Generic)

import qualified Control.Exception as E

import Github.Data.Id
import Github.Data.Name

-- | The options for querying commits.
data CommitQueryOption = CommitQuerySha !Text
                       | CommitQueryPath !Text
                       | CommitQueryAuthor !Text
                       | CommitQuerySince !UTCTime
                       | CommitQueryUntil !UTCTime
                       deriving (Show, Eq, Ord)

-- | Errors have been tagged according to their source, so you can more easily
-- dispatch and handle them.
data Error =
    HTTPConnectionError E.SomeException -- ^ A HTTP error occurred. The actual caught error is included.
  | ParseError Text -- ^ An error in the parser itself.
  | JsonError Text -- ^ The JSON is malformed or unexpected.
  | UserError Text -- ^ Incorrect input.
  deriving Show

data GithubOwner = GithubUser {
   githubOwnerAvatarUrl  :: !Text
  ,githubOwnerLogin      :: !(Name GithubOwner)
  ,githubOwnerUrl        :: !Text
  ,githubOwnerId         :: !(Id GithubOwner)
  ,githubOwnerGravatarId :: !(Maybe Text)
  }
  | GithubOrganization {
   githubOwnerAvatarUrl :: !Text
  ,githubOwnerLogin     :: !(Name GithubOwner)
  ,githubOwnerUrl       :: !Text
  ,githubOwnerId        :: !(Id GithubOwner)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubOwner

data Stats = Stats {
   statsAdditions :: !Int
  ,statsTotal     :: !Int
  ,statsDeletions :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats

data Comment = Comment {
   commentPosition  :: !(Maybe Int)
  ,commentLine      :: !(Maybe Int)
  ,commentBody      :: !Text
  ,commentCommitId  :: !(Maybe Text)
  ,commentUpdatedAt :: !UTCTime
  ,commentHtmlUrl   :: !(Maybe Text)
  ,commentUrl       :: !Text
  ,commentCreatedAt :: !(Maybe UTCTime)
  ,commentPath      :: !(Maybe Text)
  ,commentUser      :: !GithubOwner
  ,commentId        :: !(Id Comment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Comment

data NewComment = NewComment {
   newCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewComment

data EditComment = EditComment {
   editCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditComment

data SimpleOrganization = SimpleOrganization {
   simpleOrganizationUrl       :: !Text
  ,simpleOrganizationAvatarUrl :: !Text
  ,simpleOrganizationId        :: !(Id Organization)
  ,simpleOrganizationLogin     :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization

data Organization = Organization {
   organizationType        :: !Text
  ,organizationBlog        :: !(Maybe Text)
  ,organizationLocation    :: !(Maybe Text)
  ,organizationLogin       :: !(Name Organization)
  ,organizationFollowers   :: !Int
  ,organizationCompany     :: !(Maybe Text)
  ,organizationAvatarUrl   :: !Text
  ,organizationPublicGists :: !Int
  ,organizationHtmlUrl     :: !Text
  ,organizationEmail       :: !(Maybe Text)
  ,organizationFollowing   :: !Int
  ,organizationPublicRepos :: !Int
  ,organizationUrl         :: !Text
  ,organizationCreatedAt   :: !UTCTime
  ,organizationName        :: !(Maybe Text)
  ,organizationId          :: !(Id Organization)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Organization

data Content
  = ContentFile ContentFileData
  | ContentDirectory (Vector ContentItem)
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content

data ContentFileData = ContentFileData {
   contentFileInfo     :: !ContentInfo
  ,contentFileEncoding :: !Text
  ,contentFileSize     :: !Int
  ,contentFileContent  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentFileData

-- | An item in a directory listing.
data ContentItem = ContentItem {
   contentItemType :: !ContentItemType
  ,contentItemInfo :: !ContentInfo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItem

data ContentItemType = ItemFile | ItemDir
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItemType

-- | Information common to both kinds of Content: files and directories.
data ContentInfo = ContentInfo {
   contentName    :: !Text
  ,contentPath    :: !Text
  ,contentSha     :: !Text
  ,contentUrl     :: !Text
  ,contentGitUrl  :: !Text
  ,contentHtmlUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo

data Contributor
  -- | An existing Github user, with their number of contributions, avatar
  -- URL, login, URL, ID, and Gravatar ID.
  = KnownContributor Int Text (Name Contributor) Text (Id Contributor) Text
  -- | An unknown Github user with their number of contributions and recorded name.
  | AnonymousContributor Int Text
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Contributor

data DetailedOwner = DetailedUser {
   detailedOwnerCreatedAt   :: !UTCTime
  ,detailedOwnerType        :: !Text
  ,detailedOwnerPublicGists :: !Int
  ,detailedOwnerAvatarUrl   :: !Text
  ,detailedOwnerFollowers   :: !Int
  ,detailedOwnerFollowing   :: !Int
  ,detailedOwnerHireable    :: !(Maybe Bool)
  ,detailedOwnerGravatarId  :: !(Maybe Text)
  ,detailedOwnerBlog        :: !(Maybe Text)
  ,detailedOwnerBio         :: !(Maybe Text)
  ,detailedOwnerPublicRepos :: !Int
  ,detailedOwnerName        :: !(Maybe Text)
  ,detailedOwnerLocation    :: !(Maybe Text)
  ,detailedOwnerCompany     :: !(Maybe Text)
  ,detailedOwnerEmail       :: !(Maybe Text)
  ,detailedOwnerUrl         :: !Text
  ,detailedOwnerId          :: !(Id GithubOwner)
  ,detailedOwnerHtmlUrl     :: !Text
  ,detailedOwnerLogin       :: !(Name GithubOwner)
  }
  | DetailedOrganization {
   detailedOwnerCreatedAt   :: !UTCTime
  ,detailedOwnerType        :: !Text
  ,detailedOwnerPublicGists :: !Int
  ,detailedOwnerAvatarUrl   :: !Text
  ,detailedOwnerFollowers   :: !Int
  ,detailedOwnerFollowing   :: !Int
  ,detailedOwnerBlog        :: !(Maybe Text)
  ,detailedOwnerBio         :: !(Maybe Text)
  ,detailedOwnerPublicRepos :: !Int
  ,detailedOwnerName        :: !(Maybe Text)
  ,detailedOwnerLocation    :: !(Maybe Text)
  ,detailedOwnerCompany     :: !(Maybe Text)
  ,detailedOwnerUrl         :: !Text
  ,detailedOwnerId          :: !(Id GithubOwner)
  ,detailedOwnerHtmlUrl     :: !Text
  ,detailedOwnerLogin       :: !(Name GithubOwner)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedOwner
