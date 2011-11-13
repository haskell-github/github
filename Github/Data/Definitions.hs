{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Github.Data.Definitions where

import Data.Time
import Data.Data
import Network.HTTP.Enumerator (HttpException(..))
import qualified Control.Exception as E

deriving instance Eq Network.HTTP.Enumerator.HttpException

data Error =
    HTTPConnectionError E.IOException
  | ParseError String
  | JsonError String
  deriving (Show, Eq)

newtype GithubDate = GithubDate { fromGithubDate :: UTCTime }
  deriving (Show, Data, Typeable, Eq, Ord)

data Commit = Commit {
   commitSha       :: String
  ,commitParents   :: [Tree]
  ,commitUrl       :: String
  ,commitGitCommit :: GitCommit
  ,commitCommitter :: Maybe GithubUser
  ,commitAuthor    :: Maybe GithubUser
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
  ,gitTreeUrl :: String
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

data GithubUser = GithubUser {
   githubUserAvatarUrl :: String
  ,githubUserLogin :: String
  ,githubUserUrl :: String
  ,githubUserId :: Int
  ,githubUserGravatarId :: String
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
  ,commentCommitId :: String
  ,commentUpdatedAt :: UTCTime
  ,commentHtmlUrl :: String
  ,commentUrl :: String
  ,commentCreatedAt :: UTCTime
  ,commentPath :: Maybe String
  ,commentUser :: GithubUser
  ,commentId :: Int
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
   gistUser :: GithubUser
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
   gistCommentUser :: GithubUser
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
  ,issueHtmlUrl :: String
  ,issueClosedBy :: Maybe String
  ,issueLabels :: [IssueLabel]
  ,issueNumber :: Int
  ,issueAssignee :: Maybe GithubUser
  ,issueUser :: GithubUser
  ,issueTitle :: String
  ,issuePullRequest :: PullRequest
  ,issueUrl :: String
  ,issueCreatedAt :: GithubDate
  ,issueBody :: String
  ,issueState :: String
  ,issueId :: Int
  ,issueComments :: Int
  ,issueMilestone :: Maybe Milestone
} deriving (Show, Data, Typeable, Eq, Ord)

data Milestone = Milestone {
   milestoneCreator :: GithubUser
  ,milestoneDueOn :: GithubDate
  ,milestoneOpenIssues :: Int
  ,milestoneNumber :: Int
  ,milestoneClosedIssues :: Int
  ,milestoneDescription :: String
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

data PullRequest = PullRequest {
  pullRequestHtmlUrl :: Maybe String
  ,pullRequestPatchUrl :: Maybe String
  ,pullRequestDiffUrl :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord)

data IssueComment = IssueComment {
   issueCommentUpdatedAt :: GithubDate
  ,issueCommentUser :: GithubUser
  ,issueCommentUrl :: String
  ,issueCommentCreatedAt :: GithubDate
  ,issueCommentBody :: String
  ,issueCommentId :: Int
} deriving (Show, Data, Typeable, Eq, Ord)

data EventType =
    Mentioned     -- | The actor was @mentioned in an issue body.
  | Subscribed    -- | The actor subscribed to receive notifications for an issue.
  | Unsubscribed  -- | The issue was unsubscribed from by the actor.
  | Referenced    -- | The issue was referenced from a commit message. The commit_id attribute is the commit SHA1 of where that happened.
  | Merged        -- | The issue was merged by the actor. The commit_id attribute is the SHA1 of the HEAD commit that was merged.
  | Assigned      -- | The issue was assigned to the actor.
  | Closed        -- | The issue was closed by the actor. When the commit_id is present, it identifies the commit that closed the issue using “closes / fixes #NN” syntax. 
  | Reopened      -- | The issue was reopened by the actor.
  deriving (Show, Data, Typeable, Eq, Ord)

data Event = Event {
   eventActor :: GithubUser
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
