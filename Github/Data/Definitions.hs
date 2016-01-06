{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Github.Data.Definitions where

import           Control.DeepSeq   (NFData)
import qualified Control.Exception as E
import           Data.Data
import           Data.Time
import           GHC.Generics      (Generic)

import Github.Data.Id
import Github.Data.Name

-- | The options for querying commits.
data CommitQueryOption = CommitQuerySha String
                       | CommitQueryPath String
                       | CommitQueryAuthor String
                       | CommitQuerySince GithubDate
                       | CommitQueryUntil GithubDate
                       deriving (Show, Eq, Ord)

-- | Errors have been tagged according to their source, so you can more easily
-- dispatch and handle them.
data Error =
    HTTPConnectionError E.SomeException -- ^ A HTTP error occurred. The actual caught error is included.
  | ParseError String -- ^ An error in the parser itself.
  | JsonError String -- ^ The JSON is malformed or unexpected.
  | UserError String -- ^ Incorrect input.
  deriving Show

-- | A date in the Github format, which is a special case of ISO-8601.
newtype GithubDate = GithubDate { fromGithubDate :: UTCTime }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubDate

data GithubOwner = GithubUser {
   githubOwnerAvatarUrl  :: String
  ,githubOwnerLogin      :: Name GithubOwner
  ,githubOwnerUrl        :: String
  ,githubOwnerId         :: Id GithubOwner
  ,githubOwnerGravatarId :: Maybe String
  }
  | GithubOrganization {
   githubOwnerAvatarUrl :: String
  ,githubOwnerLogin     :: Name GithubOwner
  ,githubOwnerUrl       :: String
  ,githubOwnerId        :: Id GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubOwner

data Stats = Stats {
   statsAdditions :: Int
  ,statsTotal     :: Int
  ,statsDeletions :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats

data Comment = Comment {
   commentPosition  :: Maybe Int
  ,commentLine      :: Maybe Int
  ,commentBody      :: String
  ,commentCommitId  :: Maybe String
  ,commentUpdatedAt :: UTCTime
  ,commentHtmlUrl   :: Maybe String
  ,commentUrl       :: String
  ,commentCreatedAt :: Maybe UTCTime
  ,commentPath      :: Maybe String
  ,commentUser      :: GithubOwner
  ,commentId        :: Id Comment
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Comment

data NewComment = NewComment {
   newCommentBody :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewComment

data EditComment = EditComment {
   editCommentBody :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditComment

data SimpleOrganization = SimpleOrganization {
   simpleOrganizationUrl       :: String
  ,simpleOrganizationAvatarUrl :: String
  ,simpleOrganizationId        :: Id Organization
  ,simpleOrganizationLogin     :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization

data Organization = Organization {
   organizationType        :: String
  ,organizationBlog        :: Maybe String
  ,organizationLocation    :: Maybe String
  ,organizationLogin       :: Name Organization
  ,organizationFollowers   :: Int
  ,organizationCompany     :: Maybe String
  ,organizationAvatarUrl   :: String
  ,organizationPublicGists :: Int
  ,organizationHtmlUrl     :: String
  ,organizationEmail       :: Maybe String
  ,organizationFollowing   :: Int
  ,organizationPublicRepos :: Int
  ,organizationUrl         :: String
  ,organizationCreatedAt   :: GithubDate
  ,organizationName        :: Maybe String
  ,organizationId          :: Id Organization
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Organization

data SearchReposResult = SearchReposResult {
  searchReposTotalCount :: Int
  ,searchReposRepos     :: [Repo]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchReposResult

data Repo = Repo {
   repoSshUrl          :: Maybe String
  ,repoDescription     :: Maybe String
  ,repoCreatedAt       :: Maybe GithubDate
  ,repoHtmlUrl         :: String
  ,repoSvnUrl          :: Maybe String
  ,repoForks           :: Maybe Int
  ,repoHomepage        :: Maybe String
  ,repoFork            :: Maybe Bool
  ,repoGitUrl          :: Maybe String
  ,repoPrivate         :: Bool
  ,repoCloneUrl        :: Maybe String
  ,repoSize            :: Maybe Int
  ,repoUpdatedAt       :: Maybe GithubDate
  ,repoWatchers        :: Maybe Int
  ,repoOwner           :: GithubOwner
  ,repoName            :: Name Repo
  ,repoLanguage        :: Maybe String
  ,repoMasterBranch    :: Maybe String
  ,repoPushedAt        :: Maybe GithubDate   -- ^ this is Nothing for new repositories
  ,repoId              :: Id Repo
  ,repoUrl             :: String
  ,repoOpenIssues      :: Maybe Int
  ,repoHasWiki         :: Maybe Bool
  ,repoHasIssues       :: Maybe Bool
  ,repoHasDownloads    :: Maybe Bool
  ,repoParent          :: Maybe RepoRef
  ,repoSource          :: Maybe RepoRef
  ,repoHooksUrl        :: String
  ,repoStargazersCount :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Repo

data RepoRef = RepoRef GithubOwner (Name Repo) -- Repo owner and name
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoRef

data SearchCodeResult = SearchCodeResult {
   searchCodeTotalCount :: Int
  ,searchCodeCodes      :: [Code]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchCodeResult

data Code = Code {
   codeName    :: String
  ,codePath    :: String
  ,codeSha     :: String
  ,codeUrl     :: String
  ,codeGitUrl  :: String
  ,codeHtmlUrl :: String
  ,codeRepo    :: Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Code

data Content
  = ContentFile ContentFileData
  | ContentDirectory [ContentItem]
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content

data ContentFileData = ContentFileData {
   contentFileInfo     :: ContentInfo
  ,contentFileEncoding :: String
  ,contentFileSize     :: Int
  ,contentFileContent  :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentFileData

-- | An item in a directory listing.
data ContentItem = ContentItem {
   contentItemType :: ContentItemType
  ,contentItemInfo :: ContentInfo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItem

data ContentItemType = ItemFile | ItemDir
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItemType

-- | Information common to both kinds of Content: files and directories.
data ContentInfo = ContentInfo {
   contentName    :: String
  ,contentPath    :: String
  ,contentSha     :: String
  ,contentUrl     :: String
  ,contentGitUrl  :: String
  ,contentHtmlUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo

data Contributor
  -- | An existing Github user, with their number of contributions, avatar
  -- URL, login, URL, ID, and Gravatar ID.
  = KnownContributor Int String (Name Contributor) String (Id Contributor) String
  -- | An unknown Github user with their number of contributions and recorded name.
  | AnonymousContributor Int String
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Contributor

-- | This is only used for the FromJSON instance.
data Languages = Languages { getLanguages :: [Language] }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Languages

-- | A programming language with the name and number of characters written in
-- it.
data Language = Language String Int
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Language

data DetailedOwner = DetailedUser {
   detailedOwnerCreatedAt   :: GithubDate
  ,detailedOwnerType        :: String
  ,detailedOwnerPublicGists :: Int
  ,detailedOwnerAvatarUrl   :: String
  ,detailedOwnerFollowers   :: Int
  ,detailedOwnerFollowing   :: Int
  ,detailedOwnerHireable    :: Maybe Bool
  ,detailedOwnerGravatarId  :: Maybe String
  ,detailedOwnerBlog        :: Maybe String
  ,detailedOwnerBio         :: Maybe String
  ,detailedOwnerPublicRepos :: Int
  ,detailedOwnerName        :: Maybe String
  ,detailedOwnerLocation    :: Maybe String
  ,detailedOwnerCompany     :: Maybe String
  ,detailedOwnerEmail       :: Maybe String
  ,detailedOwnerUrl         :: String
  ,detailedOwnerId          :: Id GithubOwner
  ,detailedOwnerHtmlUrl     :: String
  ,detailedOwnerLogin       :: Name GithubOwner
  }
  | DetailedOrganization {
   detailedOwnerCreatedAt   :: GithubDate
  ,detailedOwnerType        :: String
  ,detailedOwnerPublicGists :: Int
  ,detailedOwnerAvatarUrl   :: String
  ,detailedOwnerFollowers   :: Int
  ,detailedOwnerFollowing   :: Int
  ,detailedOwnerBlog        :: Maybe String
  ,detailedOwnerBio         :: Maybe String
  ,detailedOwnerPublicRepos :: Int
  ,detailedOwnerName        :: Maybe String
  ,detailedOwnerLocation    :: Maybe String
  ,detailedOwnerCompany     :: Maybe String
  ,detailedOwnerUrl         :: String
  ,detailedOwnerId          :: Id GithubOwner
  ,detailedOwnerHtmlUrl     :: String
  ,detailedOwnerLogin       :: Name GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedOwner
