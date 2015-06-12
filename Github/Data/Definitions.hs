{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Github.Data.Definitions where

import Data.Time
import Data.Data
import qualified Control.Exception as E
import qualified Data.Map as M

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
  deriving (Show, Data, Typeable, Eq, Ord)

data Commit = Commit {
   commitSha       :: String
  ,commitParents   :: [Tree]
  ,commitUrl       :: String
  ,commitGitCommit :: GitCommit
  ,commitCommitter :: Maybe GithubOwner
  ,commitAuthor    :: Maybe GithubOwner
  ,commitFiles     :: [File]
  ,commitStats     :: Maybe Stats
} deriving (Show, Data, Typeable, Eq, Ord)

data Tree = Tree {
   treeSha :: String
  ,treeUrl :: String
  ,treeGitTrees :: [GitTree]
} deriving (Show, Data, Typeable, Eq, Ord)

data GitTree = GitTree {
  gitTreeType :: String
  ,gitTreeSha :: String
  -- Can be empty for submodule
  ,gitTreeUrl :: Maybe String
  ,gitTreeSize :: Maybe Int
  ,gitTreePath :: String
  ,gitTreeMode :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data GitCommit = GitCommit {
   gitCommitMessage :: String
  ,gitCommitUrl :: String
  ,gitCommitCommitter :: GitUser
  ,gitCommitAuthor :: GitUser
  ,gitCommitTree :: Tree
  ,gitCommitSha :: Maybe String
  ,gitCommitParents :: [Tree]
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data GitUser = GitUser {
   gitUserName  :: String
  ,gitUserEmail :: String
  ,gitUserDate  :: GithubDate
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data Stats = Stats {
   statsAdditions :: Int
  ,statsTotal :: Int
  ,statsDeletions :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data NewComment = NewComment {
   newCommentBody :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data EditComment = EditComment {
   editCommentBody :: String
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data GistFile = GistFile {
   gistFileType :: String
  ,gistFileRawUrl :: String
  ,gistFileSize :: Int
  ,gistFileLanguage :: Maybe String
  ,gistFileFilename :: String
  ,gistFileContent :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord)

data GistComment = GistComment {
   gistCommentUser :: GithubOwner
  ,gistCommentUrl :: String
  ,gistCommentCreatedAt :: GithubDate
  ,gistCommentBody :: String
  ,gistCommentUpdatedAt :: GithubDate
  ,gistCommentId :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

data Blob = Blob {
   blobUrl :: String
  ,blobEncoding :: String
  ,blobContent :: String
  ,blobSha :: String
  ,blobSize :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

data NewGitReference = NewGitReference {
   newGitReferenceRef :: String
  ,newGitReferenceSha :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data GitReference = GitReference {
   gitReferenceObject :: GitObject
  ,gitReferenceUrl :: String
  ,gitReferenceRef :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data GitObject = GitObject {
   gitObjectType :: String
  ,gitObjectSha :: String
  ,gitObjectUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data NewIssue = NewIssue {
  newIssueTitle :: String
, newIssueBody :: Maybe String
, newIssueAssignee :: Maybe String
, newIssueMilestone :: Maybe Int
, newIssueLabels :: Maybe [String]
} deriving (Show, Data, Typeable, Eq, Ord)

data EditIssue = EditIssue {
  editIssueTitle :: Maybe String
, editIssueBody :: Maybe String
, editIssueAssignee :: Maybe String
, editIssueState :: Maybe String
, editIssueMilestone :: Maybe Int
, editIssueLabels :: Maybe [String]
} deriving  (Show, Data, Typeable, Eq, Ord)


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
} deriving (Show, Data, Typeable, Eq, Ord)

data IssueLabel = IssueLabel {
   labelColor :: String
  ,labelUrl :: String
  ,labelName :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data PullRequestReference = PullRequestReference {
  pullRequestReferenceHtmlUrl :: Maybe String
  ,pullRequestReferencePatchUrl :: Maybe String
  ,pullRequestReferenceDiffUrl :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord)

data IssueComment = IssueComment {
   issueCommentUpdatedAt :: GithubDate
  ,issueCommentUser :: GithubOwner
  ,issueCommentUrl :: String
  ,issueCommentHtmlUrl :: String
  ,issueCommentCreatedAt :: GithubDate
  ,issueCommentBody :: String
  ,issueCommentId :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

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
  deriving (Show, Data, Typeable, Eq, Ord)

data Event = Event {
   eventActor :: GithubOwner
  ,eventType :: EventType
  ,eventCommitId :: Maybe String
  ,eventUrl :: String
  ,eventCreatedAt :: GithubDate
  ,eventId :: Int
  ,eventIssue :: Maybe Issue
} deriving (Show, Data, Typeable, Eq, Ord)

data SimpleOrganization = SimpleOrganization {
   simpleOrganizationUrl :: String
  ,simpleOrganizationAvatarUrl :: String
  ,simpleOrganizationId :: Int
  ,simpleOrganizationLogin :: String
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: Maybe String
  ,editPullRequestBody :: Maybe String
  ,editPullRequestState :: Maybe EditPullRequestState
} deriving (Show)

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
    deriving (Show)

data PullRequestLinks = PullRequestLinks {
   pullRequestLinksReviewComments :: String
  ,pullRequestLinksComments :: String
  ,pullRequestLinksHtml :: String
  ,pullRequestLinksSelf :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: String
  ,pullRequestCommitRef :: String
  ,pullRequestCommitSha :: String
  ,pullRequestCommitUser :: GithubOwner
  ,pullRequestCommitRepo :: Repo
} deriving (Show, Data, Typeable, Eq, Ord)

data SearchReposResult = SearchReposResult {
  searchReposTotalCount :: Int
  ,searchReposRepos :: [Repo]
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

data RepoRef = RepoRef GithubOwner String -- Repo owner and name
 deriving (Show, Data, Typeable, Eq, Ord)

data SearchCodeResult = SearchCodeResult {
  searchCodeTotalCount :: Int
  ,searchCodeCodes :: [Code]
} deriving (Show, Data, Typeable, Eq, Ord)

data Code = Code {
   codeName :: String
  ,codePath :: String
  ,codeSha :: String
  ,codeUrl :: String
  ,codeGitUrl :: String
  ,codeHtmlUrl :: String
  ,codeRepo :: Repo
} deriving (Show, Data, Typeable, Eq, Ord)

data Content = ContentFile ContentData | ContentDirectory [ContentData]
 deriving (Show, Data, Typeable, Eq, Ord)

data ContentData = ContentData {
   contentType :: String
  ,contentEncoding :: String
  ,contentSize :: Int
  ,contentName :: String
  ,contentPath :: String
  ,contentData :: String
  ,contentSha :: String
  ,contentUrl :: String
  ,contentGitUrl :: String
  ,contentHtmlUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord)

data Contributor
  -- | An existing Github user, with their number of contributions, avatar
  -- URL, login, URL, ID, and Gravatar ID.
  = KnownContributor Int String String String Int String
  -- | An unknown Github user with their number of contributions and recorded name.
  | AnonymousContributor Int String
 deriving (Show, Data, Typeable, Eq, Ord)

-- | This is only used for the FromJSON instance.
data Languages = Languages { getLanguages :: [Language] }
  deriving (Show, Data, Typeable, Eq, Ord)

-- | A programming language with the name and number of characters written in
-- it.
data Language = Language String Int
 deriving (Show, Data, Typeable, Eq, Ord)

data Tag = Tag {
   tagName :: String
  ,tagZipballUrl :: String
  ,tagTarballUrl :: String
  ,tagCommit :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord)

data Branch = Branch {
   branchName :: String
  ,branchCommit :: BranchCommit
} deriving (Show, Data, Typeable, Eq, Ord)

data BranchCommit = BranchCommit {
   branchCommitSha :: String
  ,branchCommitUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

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
} deriving (Show, Data, Typeable, Eq, Ord)

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
   deriving (Show, Data, Typeable, Eq, Ord)

data RepoWebhookResponse = RepoWebhookResponse {
   repoWebhookResponseCode :: Maybe Int
  ,repoWebhookResponseStatus :: String
  ,repoWebhookResponseMessage :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord)

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction :: PullRequestEventType
  ,pullRequestEventNumber :: Int
  ,pullRequestEventPullRequest :: DetailedPullRequest
  ,pullRequestRepository :: Repo
  ,pullRequestSender :: GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord)

data PullRequestEventType =
    PullRequestOpened
  | PullRequestClosed
  | PullRequestSynchronized
  | PullRequestReopened
  | PullRequestAssigned
  | PullRequestUnassigned
  | PullRequestLabeled
  | PullRequestUnlabeled
  deriving (Show, Data, Typeable, Eq, Ord)

data PingEvent = PingEvent {
   pingEventZen :: String
  ,pingEventHook :: RepoWebhook
  ,pingEventHookId :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving Show
