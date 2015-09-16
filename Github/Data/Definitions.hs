{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Github.Data.Definitions where

import Control.DeepSeq (NFData)
import Data.Time
import Data.Data
import GHC.Generics (Generic)
import qualified Control.Exception as E
import qualified Data.Map as M

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
   treeSha :: String
  ,treeUrl :: String
  ,treeGitTrees :: [GitTree]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tree

data GitTree = GitTree {
  gitTreeType :: String
  ,gitTreeSha :: String
  -- Can be empty for submodule
  ,gitTreeUrl :: Maybe String
  ,gitTreeSize :: Maybe Int
  ,gitTreePath :: String
  ,gitTreeMode :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitTree

data GitCommit = GitCommit {
   gitCommitMessage :: String
  ,gitCommitUrl :: String
  ,gitCommitCommitter :: GitUser
  ,gitCommitAuthor :: GitUser
  ,gitCommitTree :: Tree
  ,gitCommitSha :: Maybe String
  ,gitCommitParents :: [Tree]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitCommit

data GithubOwner = GithubUser {
   githubOwnerAvatarUrl :: String
  ,githubOwnerLogin :: String
  ,githubOwnerUrl :: String
  ,githubOwnerId :: Int
  ,githubOwnerGravatarId :: Maybe String
  }
  | GithubOrganization {
   githubOwnerAvatarUrl :: String
  ,githubOwnerLogin :: String
  ,githubOwnerUrl :: String
  ,githubOwnerId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubOwner

data GitUser = GitUser {
   gitUserName  :: String
  ,gitUserEmail :: String
  ,gitUserDate  :: GithubDate
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitUser

data File = File {
   fileBlobUrl :: String
  ,fileStatus :: String
  ,fileRawUrl :: String
  ,fileAdditions :: Int
  ,fileSha :: String
  ,fileChanges :: Int
  ,filePatch :: String
  ,fileFilename :: String
  ,fileDeletions :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData File

data Stats = Stats {
   statsAdditions :: Int
  ,statsTotal :: Int
  ,statsDeletions :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Stats

data Comment = Comment {
   commentPosition :: Maybe Int
  ,commentLine :: Maybe Int
  ,commentBody :: String
  ,commentCommitId :: Maybe String
  ,commentUpdatedAt :: UTCTime
  ,commentHtmlUrl :: Maybe String
  ,commentUrl :: String
  ,commentCreatedAt :: Maybe UTCTime
  ,commentPath :: Maybe String
  ,commentUser :: GithubOwner
  ,commentId :: Int
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

data Diff = Diff {
   diffStatus :: String
  ,diffBehindBy :: Int
  ,diffPatchUrl :: String
  ,diffUrl :: String
  ,diffBaseCommit :: Commit
  ,diffCommits :: [Commit]
  ,diffTotalCommits :: Int
  ,diffHtmlUrl :: String
  ,diffFiles :: [File]
  ,diffAheadBy :: Int
  ,diffDiffUrl :: String
  ,diffPermalinkUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Diff

data Gist = Gist {
   gistUser :: GithubOwner
  ,gistGitPushUrl :: String
  ,gistUrl :: String
  ,gistDescription :: Maybe String
  ,gistCreatedAt :: GithubDate
  ,gistPublic :: Bool
  ,gistComments :: Int
  ,gistUpdatedAt :: GithubDate
  ,gistHtmlUrl :: String
  ,gistId :: String
  ,gistFiles :: [GistFile]
  ,gistGitPullUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Gist

data GistFile = GistFile {
   gistFileType :: String
  ,gistFileRawUrl :: String
  ,gistFileSize :: Int
  ,gistFileLanguage :: Maybe String
  ,gistFileFilename :: String
  ,gistFileContent :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistFile

data GistComment = GistComment {
   gistCommentUser :: GithubOwner
  ,gistCommentUrl :: String
  ,gistCommentCreatedAt :: GithubDate
  ,gistCommentBody :: String
  ,gistCommentUpdatedAt :: GithubDate
  ,gistCommentId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment

data Blob = Blob {
   blobUrl :: String
  ,blobEncoding :: String
  ,blobContent :: String
  ,blobSha :: String
  ,blobSize :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Blob

data NewGitReference = NewGitReference {
   newGitReferenceRef :: String
  ,newGitReferenceSha :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewGitReference

data GitReference = GitReference {
   gitReferenceObject :: GitObject
  ,gitReferenceUrl :: String
  ,gitReferenceRef :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitReference

data GitObject = GitObject {
   gitObjectType :: String
  ,gitObjectSha :: String
  ,gitObjectUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GitObject

data Issue = Issue {
   issueClosedAt :: Maybe GithubDate
  ,issueUpdatedAt :: GithubDate
  ,issueEventsUrl :: String
  ,issueHtmlUrl :: Maybe String
  ,issueClosedBy :: Maybe GithubOwner
  ,issueLabels :: [IssueLabel]
  ,issueNumber :: Int
  ,issueAssignee :: Maybe GithubOwner
  ,issueUser :: GithubOwner
  ,issueTitle :: String
  ,issuePullRequest :: Maybe PullRequestReference
  ,issueUrl :: String
  ,issueCreatedAt :: GithubDate
  ,issueBody :: Maybe String
  ,issueState :: String
  ,issueId :: Int
  ,issueComments :: Int
  ,issueMilestone :: Maybe Milestone
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Issue

data NewIssue = NewIssue {
  newIssueTitle :: String
, newIssueBody :: Maybe String
, newIssueAssignee :: Maybe String
, newIssueMilestone :: Maybe Int
, newIssueLabels :: Maybe [String]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewIssue

data EditIssue = EditIssue {
  editIssueTitle :: Maybe String
, editIssueBody :: Maybe String
, editIssueAssignee :: Maybe String
, editIssueState :: Maybe String
, editIssueMilestone :: Maybe Int
, editIssueLabels :: Maybe [String]
} deriving  (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditIssue

data Milestone = Milestone {
   milestoneCreator :: GithubOwner
  ,milestoneDueOn :: Maybe GithubDate
  ,milestoneOpenIssues :: Int
  ,milestoneNumber :: Int
  ,milestoneClosedIssues :: Int
  ,milestoneDescription :: Maybe String
  ,milestoneTitle :: String
  ,milestoneUrl :: String
  ,milestoneCreatedAt :: GithubDate
  ,milestoneState :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Milestone

data IssueLabel = IssueLabel {
   labelColor :: String
  ,labelUrl :: String
  ,labelName :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueLabel

data PullRequestReference = PullRequestReference {
  pullRequestReferenceHtmlUrl :: Maybe String
  ,pullRequestReferencePatchUrl :: Maybe String
  ,pullRequestReferenceDiffUrl :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReference

data IssueComment = IssueComment {
   issueCommentUpdatedAt :: GithubDate
  ,issueCommentUser :: GithubOwner
  ,issueCommentUrl :: String
  ,issueCommentHtmlUrl :: String
  ,issueCommentCreatedAt :: GithubDate
  ,issueCommentBody :: String
  ,issueCommentId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueComment

-- | Data describing an @Event@.
data EventType =
    Mentioned     -- ^ The actor was @mentioned in an issue body.
  | Subscribed    -- ^ The actor subscribed to receive notifications for an issue.
  | Unsubscribed  -- ^ The issue was unsubscribed from by the actor.
  | Referenced    -- ^ The issue was referenced from a commit message. The commit_id attribute is the commit SHA1 of where that happened.
  | Merged        -- ^ The issue was merged by the actor. The commit_id attribute is the SHA1 of the HEAD commit that was merged.
  | Assigned      -- ^ The issue was assigned to the actor.
  | Closed        -- ^ The issue was closed by the actor. When the commit_id is present, it identifies the commit that closed the issue using “closes / fixes #NN” syntax. 
  | Reopened      -- ^ The issue was reopened by the actor.
  | ActorUnassigned    -- ^ The issue was unassigned to the actor
  | Labeled       -- ^ A label was added to the issue.
  | Unlabeled     -- ^ A label was removed from the issue.
  | Milestoned    -- ^ The issue was added to a milestone.
  | Demilestoned  -- ^ The issue was removed from a milestone.
  | Renamed       -- ^ The issue title was changed.
  | Locked        -- ^ The issue was locked by the actor.
  | Unlocked      -- ^ The issue was unlocked by the actor.
  | HeadRefDeleted -- ^ The pull request’s branch was deleted.
  | HeadRefRestored -- ^ The pull request’s branch was restored.
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EventType

data Event = Event {
   eventActor :: GithubOwner
  ,eventType :: EventType
  ,eventCommitId :: Maybe String
  ,eventUrl :: String
  ,eventCreatedAt :: GithubDate
  ,eventId :: Int
  ,eventIssue :: Maybe Issue
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Event

data SimpleOrganization = SimpleOrganization {
   simpleOrganizationUrl :: String
  ,simpleOrganizationAvatarUrl :: String
  ,simpleOrganizationId :: Int
  ,simpleOrganizationLogin :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization

data Organization = Organization {
   organizationType :: String
  ,organizationBlog :: Maybe String
  ,organizationLocation :: Maybe String
  ,organizationLogin :: String
  ,organizationFollowers :: Int
  ,organizationCompany :: Maybe String
  ,organizationAvatarUrl :: String
  ,organizationPublicGists :: Int
  ,organizationHtmlUrl :: String
  ,organizationEmail :: Maybe String
  ,organizationFollowing :: Int
  ,organizationPublicRepos :: Int
  ,organizationUrl :: String
  ,organizationCreatedAt :: GithubDate
  ,organizationName :: Maybe String
  ,organizationId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Organization

data PullRequest = PullRequest {
   pullRequestClosedAt :: Maybe GithubDate
  ,pullRequestCreatedAt :: GithubDate
  ,pullRequestUser :: GithubOwner
  ,pullRequestPatchUrl :: String
  ,pullRequestState :: String
  ,pullRequestNumber :: Int
  ,pullRequestHtmlUrl :: String
  ,pullRequestUpdatedAt :: GithubDate
  ,pullRequestBody :: String
  ,pullRequestIssueUrl :: String
  ,pullRequestDiffUrl :: String
  ,pullRequestUrl :: String
  ,pullRequestLinks :: PullRequestLinks
  ,pullRequestMergedAt :: Maybe GithubDate
  ,pullRequestTitle :: String
  ,pullRequestId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest

data DetailedPullRequest = DetailedPullRequest {
  -- this is a duplication of a PullRequest
   detailedPullRequestClosedAt :: Maybe GithubDate
  ,detailedPullRequestCreatedAt :: GithubDate
  ,detailedPullRequestUser :: GithubOwner
  ,detailedPullRequestPatchUrl :: String
  ,detailedPullRequestState :: String
  ,detailedPullRequestNumber :: Int
  ,detailedPullRequestHtmlUrl :: String
  ,detailedPullRequestUpdatedAt :: GithubDate
  ,detailedPullRequestBody :: String
  ,detailedPullRequestIssueUrl :: String
  ,detailedPullRequestDiffUrl :: String
  ,detailedPullRequestUrl :: String
  ,detailedPullRequestLinks :: PullRequestLinks
  ,detailedPullRequestMergedAt :: Maybe GithubDate
  ,detailedPullRequestTitle :: String
  ,detailedPullRequestId :: Int

  ,detailedPullRequestMergedBy :: Maybe GithubOwner
  ,detailedPullRequestChangedFiles :: Int
  ,detailedPullRequestHead :: PullRequestCommit
  ,detailedPullRequestComments :: Int
  ,detailedPullRequestDeletions :: Int
  ,detailedPullRequestAdditions :: Int
  ,detailedPullRequestReviewComments :: Int
  ,detailedPullRequestBase :: PullRequestCommit
  ,detailedPullRequestCommits :: Int
  ,detailedPullRequestMerged :: Bool
  ,detailedPullRequestMergeable :: Maybe Bool
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedPullRequest

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: Maybe String
  ,editPullRequestBody :: Maybe String
  ,editPullRequestState :: Maybe EditPullRequestState
} deriving (Show, Generic)

instance NFData EditPullRequest

data CreatePullRequest =
      CreatePullRequest
      { createPullRequestTitle :: String
      , createPullRequestBody  :: String
      , createPullRequestHead  :: String
      , createPullRequestBase  :: String
      }
    | CreatePullRequestIssue
      { createPullRequestIssueNum :: Int
      , createPullRequestHead     :: String
      , createPullRequestBase     :: String
      }
    deriving (Show, Generic)

instance NFData CreatePullRequest

data PullRequestLinks = PullRequestLinks {
   pullRequestLinksReviewComments :: String
  ,pullRequestLinksComments :: String
  ,pullRequestLinksHtml :: String
  ,pullRequestLinksSelf :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: String
  ,pullRequestCommitRef :: String
  ,pullRequestCommitSha :: String
  ,pullRequestCommitUser :: GithubOwner
  ,pullRequestCommitRepo :: Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit

data SearchReposResult = SearchReposResult {
  searchReposTotalCount :: Int
  ,searchReposRepos :: [Repo]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchReposResult

data Repo = Repo {
   repoSshUrl :: Maybe String
  ,repoDescription :: Maybe String
  ,repoCreatedAt :: Maybe GithubDate
  ,repoHtmlUrl :: String
  ,repoSvnUrl :: Maybe String
  ,repoForks :: Maybe Int
  ,repoHomepage :: Maybe String
  ,repoFork :: Maybe Bool
  ,repoGitUrl :: Maybe String
  ,repoPrivate :: Bool
  ,repoCloneUrl :: Maybe String
  ,repoSize :: Maybe Int
  ,repoUpdatedAt :: Maybe GithubDate
  ,repoWatchers :: Maybe Int
  ,repoOwner :: GithubOwner
  ,repoName :: String
  ,repoLanguage :: Maybe String
  ,repoMasterBranch :: Maybe String
  ,repoPushedAt :: Maybe GithubDate   -- ^ this is Nothing for new repositories
  ,repoId :: Int
  ,repoUrl :: String
  ,repoOpenIssues :: Maybe Int
  ,repoHasWiki :: Maybe Bool
  ,repoHasIssues :: Maybe Bool
  ,repoHasDownloads :: Maybe Bool
  ,repoParent :: Maybe RepoRef
  ,repoSource :: Maybe RepoRef
  ,repoHooksUrl :: String
  ,repoStargazersCount :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Repo

data RepoRef = RepoRef GithubOwner String -- Repo owner and name
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoRef

data SearchCodeResult = SearchCodeResult {
   searchCodeTotalCount :: Int
  ,searchCodeCodes :: [Code]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchCodeResult

data Code = Code {
   codeName :: String
  ,codePath :: String
  ,codeSha :: String
  ,codeUrl :: String
  ,codeGitUrl :: String
  ,codeHtmlUrl :: String
  ,codeRepo :: Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Code

data Content
  = ContentFile ContentFileData
  | ContentDirectory [ContentItem]
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content

data ContentFileData = ContentFileData {
   contentFileInfo :: ContentInfo
  ,contentFileEncoding :: String
  ,contentFileSize :: Int
  ,contentFileContent :: String
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
   contentName :: String
  ,contentPath :: String
  ,contentSha :: String
  ,contentUrl :: String
  ,contentGitUrl :: String
  ,contentHtmlUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo

data Contributor
  -- | An existing Github user, with their number of contributions, avatar
  -- URL, login, URL, ID, and Gravatar ID.
  = KnownContributor Int String String String Int String
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

data Tag = Tag {
   tagName :: String
  ,tagZipballUrl :: String
  ,tagTarballUrl :: String
  ,tagCommit :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Tag

data Branch = Branch {
   branchName :: String
  ,branchCommit :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Branch

data BranchCommit = BranchCommit {
   branchCommitSha :: String
  ,branchCommitUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData BranchCommit

data DetailedOwner = DetailedUser {
   detailedOwnerCreatedAt :: GithubDate
  ,detailedOwnerType :: String
  ,detailedOwnerPublicGists :: Int
  ,detailedOwnerAvatarUrl :: String
  ,detailedOwnerFollowers :: Int
  ,detailedOwnerFollowing :: Int
  ,detailedOwnerHireable :: Maybe Bool
  ,detailedOwnerGravatarId :: Maybe String
  ,detailedOwnerBlog :: Maybe String
  ,detailedOwnerBio :: Maybe String
  ,detailedOwnerPublicRepos :: Int
  ,detailedOwnerName :: Maybe String
  ,detailedOwnerLocation :: Maybe String
  ,detailedOwnerCompany :: Maybe String
  ,detailedOwnerEmail :: Maybe String
  ,detailedOwnerUrl :: String
  ,detailedOwnerId :: Int
  ,detailedOwnerHtmlUrl :: String
  ,detailedOwnerLogin :: String
  }
  | DetailedOrganization {
   detailedOwnerCreatedAt :: GithubDate
  ,detailedOwnerType :: String
  ,detailedOwnerPublicGists :: Int
  ,detailedOwnerAvatarUrl :: String
  ,detailedOwnerFollowers :: Int
  ,detailedOwnerFollowing :: Int
  ,detailedOwnerBlog :: Maybe String
  ,detailedOwnerBio :: Maybe String
  ,detailedOwnerPublicRepos :: Int
  ,detailedOwnerName :: Maybe String
  ,detailedOwnerLocation :: Maybe String
  ,detailedOwnerCompany :: Maybe String
  ,detailedOwnerUrl :: String
  ,detailedOwnerId :: Int
  ,detailedOwnerHtmlUrl :: String
  ,detailedOwnerLogin :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedOwner

data RepoWebhook = RepoWebhook {
   repoWebhookUrl :: String
  ,repoWebhookTestUrl :: String
  ,repoWebhookId :: Integer
  ,repoWebhookName :: String
  ,repoWebhookActive :: Bool
  ,repoWebhookEvents :: [RepoWebhookEvent]
  ,repoWebhookConfig :: M.Map String String
  ,repoWebhookLastResponse :: RepoWebhookResponse
  ,repoWebhookUpdatedAt :: GithubDate
  ,repoWebhookCreatedAt :: GithubDate
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhook

data RepoWebhookEvent =
   WebhookWildcardEvent
 | WebhookCommitCommentEvent
 | WebhookCreateEvent
 | WebhookDeleteEvent
 | WebhookDeploymentEvent
 | WebhookDeploymentStatusEvent
 | WebhookForkEvent
 | WebhookGollumEvent
 | WebhookIssueCommentEvent
 | WebhookIssuesEvent
 | WebhookMemberEvent
 | WebhookPageBuildEvent
 | WebhookPublicEvent
 | WebhookPullRequestReviewCommentEvent
 | WebhookPullRequestEvent
 | WebhookPushEvent
 | WebhookReleaseEvent
 | WebhookStatusEvent
 | WebhookTeamAddEvent
 | WebhookWatchEvent
   deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookEvent

data RepoWebhookResponse = RepoWebhookResponse {
   repoWebhookResponseCode :: Maybe Int
  ,repoWebhookResponseStatus :: String
  ,repoWebhookResponseMessage :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookResponse

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction :: PullRequestEventType
  ,pullRequestEventNumber :: Int
  ,pullRequestEventPullRequest :: DetailedPullRequest
  ,pullRequestRepository :: Repo
  ,pullRequestSender :: GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEvent

data PullRequestEventType =
    PullRequestOpened
  | PullRequestClosed
  | PullRequestSynchronized
  | PullRequestReopened
  | PullRequestAssigned
  | PullRequestUnassigned
  | PullRequestLabeled
  | PullRequestUnlabeled
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEventType

data PingEvent = PingEvent {
   pingEventZen :: String
  ,pingEventHook :: RepoWebhook
  ,pingEventHookId :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PingEvent

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving (Show, Generic)

instance NFData EditPullRequestState
