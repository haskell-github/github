{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Github.Data.Definitions where

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)
import Network.HTTP.Client      (HttpException)

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
    HTTPError !HttpException -- ^ A HTTP error occurred. The actual caught error is included.
  | ParseError !Text -- ^ An error in the parser itself.
  | JsonError !Text -- ^ The JSON is malformed or unexpected.
  | UserError !Text -- ^ Incorrect input.
  deriving (Show, Typeable)

instance E.Exception Error

data SimpleOwner = SimpleUserOwner {
   simpleOwnerAvatarUrl  :: !Text
  ,simpleOwnerLogin      :: !(Name GithubOwner)
  ,simpleOwnerUrl        :: !Text
  ,simpleOwnerId         :: !(Id GithubOwner)
  ,simpleOwnerGravatarId :: !(Maybe Text)
  }
  | SimpleOrganizationOwner {
   simpleOwnerAvatarUrl :: !Text
  ,simpleOwnerLogin     :: !(Name GithubOwner)
  ,simpleOwnerUrl       :: !Text
  ,simpleOwnerId        :: !(Id GithubOwner)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOwner where rnf = genericRnf
instance Binary SimpleOwner

data Stats = Stats {
   statsAdditions :: !Int
  ,statsTotal     :: !Int
  ,statsDeletions :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats where rnf = genericRnf
instance Binary Stats

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
  ,commentUser      :: !SimpleOwner
  ,commentId        :: !(Id Comment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Comment where rnf = genericRnf
instance Binary Comment

data NewComment = NewComment {
   newCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewComment where rnf = genericRnf
instance Binary NewComment

data EditComment = EditComment {
   editCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditComment where rnf = genericRnf
instance Binary EditComment

data SimpleOrganization = SimpleOrganization {
   simpleOrganizationUrl       :: !Text
  ,simpleOrganizationAvatarUrl :: !Text
  ,simpleOrganizationId        :: !(Id Organization)
  ,simpleOrganizationLogin     :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization where rnf = genericRnf
instance Binary SimpleOrganization

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

instance NFData Organization where rnf = genericRnf
instance Binary Organization

data Content
  = ContentFile ContentFileData
  | ContentDirectory (Vector ContentItem)
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content where rnf = genericRnf
instance Binary Content

data ContentFileData = ContentFileData {
   contentFileInfo     :: !ContentInfo
  ,contentFileEncoding :: !Text
  ,contentFileSize     :: !Int
  ,contentFileContent  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentFileData where rnf = genericRnf
instance Binary ContentFileData

-- | An item in a directory listing.
data ContentItem = ContentItem {
   contentItemType :: !ContentItemType
  ,contentItemInfo :: !ContentInfo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItem where rnf = genericRnf
instance Binary ContentItem

data ContentItemType = ItemFile | ItemDir
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItemType where rnf = genericRnf
instance Binary ContentItemType

-- | Information common to both kinds of Content: files and directories.
data ContentInfo = ContentInfo {
   contentName    :: !Text
  ,contentPath    :: !Text
  ,contentSha     :: !Text
  ,contentUrl     :: !Text
  ,contentGitUrl  :: !Text
  ,contentHtmlUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo where rnf = genericRnf
instance Binary ContentInfo

data Contributor
  -- | An existing Github user, with their number of contributions, avatar
  -- URL, login, URL, ID, and Gravatar ID.
  = KnownContributor Int Text (Name Contributor) Text (Id Contributor) Text
  -- | An unknown Github user with their number of contributions and recorded name.
  | AnonymousContributor Int Text
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Contributor where rnf = genericRnf
instance Binary Contributor

data GithubOwner = GithubUser {
   githubOwnerCreatedAt   :: !UTCTime
  ,githubOwnerType        :: !Text
  ,githubOwnerPublicGists :: !Int
  ,githubOwnerAvatarUrl   :: !Text
  ,githubOwnerFollowers   :: !Int
  ,githubOwnerFollowing   :: !Int
  ,githubOwnerHireable    :: !(Maybe Bool)
  ,githubOwnerGravatarId  :: !(Maybe Text)
  ,githubOwnerBlog        :: !(Maybe Text)
  ,githubOwnerBio         :: !(Maybe Text)
  ,githubOwnerPublicRepos :: !Int
  ,githubOwnerName        :: !(Maybe Text)
  ,githubOwnerLocation    :: !(Maybe Text)
  ,githubOwnerCompany     :: !(Maybe Text)
  ,githubOwnerEmail       :: !(Maybe Text)
  ,githubOwnerUrl         :: !Text
  ,githubOwnerId          :: !(Id GithubOwner)
  ,githubOwnerHtmlUrl     :: !Text
  ,githubOwnerLogin       :: !(Name GithubOwner)
  }
  | GithubOrganization {
   githubOwnerCreatedAt   :: !UTCTime
  ,githubOwnerType        :: !Text
  ,githubOwnerPublicGists :: !Int
  ,githubOwnerAvatarUrl   :: !Text
  ,githubOwnerFollowers   :: !Int
  ,githubOwnerFollowing   :: !Int
  ,githubOwnerBlog        :: !(Maybe Text)
  ,githubOwnerBio         :: !(Maybe Text)
  ,githubOwnerPublicRepos :: !Int
  ,githubOwnerName        :: !(Maybe Text)
  ,githubOwnerLocation    :: !(Maybe Text)
  ,githubOwnerCompany     :: !(Maybe Text)
  ,githubOwnerUrl         :: !Text
  ,githubOwnerId          :: !(Id GithubOwner)
  ,githubOwnerHtmlUrl     :: !Text
  ,githubOwnerLogin       :: !(Name GithubOwner)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubOwner where rnf = genericRnf
instance Binary GithubOwner
