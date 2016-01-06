{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Issues where

import Github.Data.Definitions
import Github.Data.Id
import Github.Data.PullRequests

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import GHC.Generics    (Generic)

data Issue = Issue {
   issueClosedAt    :: Maybe GithubDate
  ,issueUpdatedAt   :: GithubDate
  ,issueEventsUrl   :: String
  ,issueHtmlUrl     :: Maybe String
  ,issueClosedBy    :: Maybe GithubOwner
  ,issueLabels      :: [IssueLabel]
  ,issueNumber      :: Int
  ,issueAssignee    :: Maybe GithubOwner
  ,issueUser        :: GithubOwner
  ,issueTitle       :: String
  ,issuePullRequest :: Maybe PullRequestReference
  ,issueUrl         :: String
  ,issueCreatedAt   :: GithubDate
  ,issueBody        :: Maybe String
  ,issueState       :: String
  ,issueId          :: Id Issue
  ,issueComments    :: Int
  ,issueMilestone   :: Maybe Milestone
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Issue

data NewIssue = NewIssue {
  newIssueTitle     :: String
, newIssueBody      :: Maybe String
, newIssueAssignee  :: Maybe String
, newIssueMilestone :: Maybe Int
, newIssueLabels    :: Maybe [String]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewIssue

data EditIssue = EditIssue {
  editIssueTitle     :: Maybe String
, editIssueBody      :: Maybe String
, editIssueAssignee  :: Maybe String
, editIssueState     :: Maybe String
, editIssueMilestone :: Maybe Int
, editIssueLabels    :: Maybe [String]
} deriving  (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditIssue

data Milestone = Milestone {
   milestoneCreator      :: GithubOwner
  ,milestoneDueOn        :: Maybe GithubDate
  ,milestoneOpenIssues   :: Int
  ,milestoneNumber       :: Int
  ,milestoneClosedIssues :: Int
  ,milestoneDescription  :: Maybe String
  ,milestoneTitle        :: String
  ,milestoneUrl          :: String
  ,milestoneCreatedAt    :: GithubDate
  ,milestoneState        :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Milestone

data IssueLabel = IssueLabel {
   labelColor :: String
  ,labelUrl   :: String
  ,labelName  :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueLabel

data IssueComment = IssueComment {
   issueCommentUpdatedAt :: GithubDate
  ,issueCommentUser      :: GithubOwner
  ,issueCommentUrl       :: String
  ,issueCommentHtmlUrl   :: String
  ,issueCommentCreatedAt :: GithubDate
  ,issueCommentBody      :: String
  ,issueCommentId        :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueComment

data SearchIssuesResult = SearchIssuesResult {
   searchIssuesTotalCount :: Int
  ,searchIssuesIssues     :: [Issue]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchIssuesResult

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

-- | Issue event
data Event = Event {
   eventActor     :: GithubOwner
  ,eventType      :: EventType
  ,eventCommitId  :: Maybe String
  ,eventUrl       :: String
  ,eventCreatedAt :: GithubDate
  ,eventId        :: Int
  ,eventIssue     :: Maybe Issue
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Event
