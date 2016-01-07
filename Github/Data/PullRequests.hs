{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.PullRequests where

import Github.Data.Definitions
import Github.Data.Repos       (Repo)

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import GHC.Generics    (Generic)

data PullRequest = PullRequest {
   pullRequestClosedAt  :: !(Maybe GithubDate)
  ,pullRequestCreatedAt :: !GithubDate
  ,pullRequestUser      :: !GithubOwner
  ,pullRequestPatchUrl  :: !Text
  ,pullRequestState     :: !Text
  ,pullRequestNumber    :: !Int
  ,pullRequestHtmlUrl   :: !Text
  ,pullRequestUpdatedAt :: !GithubDate
  ,pullRequestBody      :: !Text
  ,pullRequestIssueUrl  :: !Text
  ,pullRequestDiffUrl   :: !Text
  ,pullRequestUrl       :: !Text
  ,pullRequestLinks     :: !PullRequestLinks
  ,pullRequestMergedAt  :: !(Maybe GithubDate)
  ,pullRequestTitle     :: !Text
  ,pullRequestId        :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest

data DetailedPullRequest = DetailedPullRequest {
  -- this is a duplication of a PullRequest
   detailedPullRequestClosedAt       :: !(Maybe GithubDate)
  ,detailedPullRequestCreatedAt      :: !GithubDate
  ,detailedPullRequestUser           :: !GithubOwner
  ,detailedPullRequestPatchUrl       :: !Text
  ,detailedPullRequestState          :: !Text
  ,detailedPullRequestNumber         :: !Int
  ,detailedPullRequestHtmlUrl        :: !Text
  ,detailedPullRequestUpdatedAt      :: !GithubDate
  ,detailedPullRequestBody           :: !Text
  ,detailedPullRequestIssueUrl       :: !Text
  ,detailedPullRequestDiffUrl        :: !Text
  ,detailedPullRequestUrl            :: !Text
  ,detailedPullRequestLinks          :: !PullRequestLinks
  ,detailedPullRequestMergedAt       :: !(Maybe GithubDate)
  ,detailedPullRequestTitle          :: !Text
  ,detailedPullRequestId             :: !Int

  ,detailedPullRequestMergedBy       :: !(Maybe GithubOwner)
  ,detailedPullRequestChangedFiles   :: !Int
  ,detailedPullRequestHead           :: !PullRequestCommit
  ,detailedPullRequestComments       :: !Int
  ,detailedPullRequestDeletions      :: !Int
  ,detailedPullRequestAdditions      :: !Int
  ,detailedPullRequestReviewComments :: !Int
  ,detailedPullRequestBase           :: !PullRequestCommit
  ,detailedPullRequestCommits        :: !Int
  ,detailedPullRequestMerged         :: !Bool
  ,detailedPullRequestMergeable      :: !(Maybe Bool)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedPullRequest

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: !(Maybe Text)
  ,editPullRequestBody  :: !(Maybe Text)
  ,editPullRequestState :: !(Maybe EditPullRequestState)
} deriving (Show, Generic)

instance NFData EditPullRequest

data CreatePullRequest =
      CreatePullRequest
      { createPullRequestTitle :: !Text
      , createPullRequestBody  :: !Text
      , createPullRequestHead  :: !Text
      , createPullRequestBase  :: !Text
      }
    | CreatePullRequestIssue
      { createPullRequestIssueNum :: !Int
      , createPullRequestHead     :: !Text
      , createPullRequestBase     :: !Text
      }
    deriving (Show, Generic)

instance NFData CreatePullRequest

data PullRequestLinks = PullRequestLinks {
   pullRequestLinksReviewComments :: !Text
  ,pullRequestLinksComments       :: !Text
  ,pullRequestLinksHtml           :: !Text
  ,pullRequestLinksSelf           :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: !Text
  ,pullRequestCommitRef   :: !Text
  ,pullRequestCommitSha   :: !Text
  ,pullRequestCommitUser  :: !GithubOwner
  ,pullRequestCommitRepo  :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction      :: !PullRequestEventType
  ,pullRequestEventNumber      :: !Int
  ,pullRequestEventPullRequest :: !DetailedPullRequest
  ,pullRequestRepository       :: !Repo
  ,pullRequestSender           :: !GithubOwner
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

data PullRequestReference = PullRequestReference {
  pullRequestReferenceHtmlUrl   :: !(Maybe Text)
  ,pullRequestReferencePatchUrl :: !(Maybe Text)
  ,pullRequestReferenceDiffUrl  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReference

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving (Show, Generic)

instance NFData EditPullRequestState
