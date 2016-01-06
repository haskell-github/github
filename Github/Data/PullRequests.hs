{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.PullRequests where

import Github.Data.Definitions

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import GHC.Generics    (Generic)

data PullRequest = PullRequest {
   pullRequestClosedAt  :: Maybe GithubDate
  ,pullRequestCreatedAt :: GithubDate
  ,pullRequestUser      :: GithubOwner
  ,pullRequestPatchUrl  :: String
  ,pullRequestState     :: String
  ,pullRequestNumber    :: Int
  ,pullRequestHtmlUrl   :: String
  ,pullRequestUpdatedAt :: GithubDate
  ,pullRequestBody      :: String
  ,pullRequestIssueUrl  :: String
  ,pullRequestDiffUrl   :: String
  ,pullRequestUrl       :: String
  ,pullRequestLinks     :: PullRequestLinks
  ,pullRequestMergedAt  :: Maybe GithubDate
  ,pullRequestTitle     :: String
  ,pullRequestId        :: Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest

data DetailedPullRequest = DetailedPullRequest {
  -- this is a duplication of a PullRequest
   detailedPullRequestClosedAt       :: Maybe GithubDate
  ,detailedPullRequestCreatedAt      :: GithubDate
  ,detailedPullRequestUser           :: GithubOwner
  ,detailedPullRequestPatchUrl       :: String
  ,detailedPullRequestState          :: String
  ,detailedPullRequestNumber         :: Int
  ,detailedPullRequestHtmlUrl        :: String
  ,detailedPullRequestUpdatedAt      :: GithubDate
  ,detailedPullRequestBody           :: String
  ,detailedPullRequestIssueUrl       :: String
  ,detailedPullRequestDiffUrl        :: String
  ,detailedPullRequestUrl            :: String
  ,detailedPullRequestLinks          :: PullRequestLinks
  ,detailedPullRequestMergedAt       :: Maybe GithubDate
  ,detailedPullRequestTitle          :: String
  ,detailedPullRequestId             :: Int

  ,detailedPullRequestMergedBy       :: Maybe GithubOwner
  ,detailedPullRequestChangedFiles   :: Int
  ,detailedPullRequestHead           :: PullRequestCommit
  ,detailedPullRequestComments       :: Int
  ,detailedPullRequestDeletions      :: Int
  ,detailedPullRequestAdditions      :: Int
  ,detailedPullRequestReviewComments :: Int
  ,detailedPullRequestBase           :: PullRequestCommit
  ,detailedPullRequestCommits        :: Int
  ,detailedPullRequestMerged         :: Bool
  ,detailedPullRequestMergeable      :: Maybe Bool
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedPullRequest

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: Maybe String
  ,editPullRequestBody  :: Maybe String
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
  ,pullRequestLinksComments       :: String
  ,pullRequestLinksHtml           :: String
  ,pullRequestLinksSelf           :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: String
  ,pullRequestCommitRef   :: String
  ,pullRequestCommitSha   :: String
  ,pullRequestCommitUser  :: GithubOwner
  ,pullRequestCommitRepo  :: Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction      :: PullRequestEventType
  ,pullRequestEventNumber      :: Int
  ,pullRequestEventPullRequest :: DetailedPullRequest
  ,pullRequestRepository       :: Repo
  ,pullRequestSender           :: GithubOwner
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
  pullRequestReferenceHtmlUrl   :: Maybe String
  ,pullRequestReferencePatchUrl :: Maybe String
  ,pullRequestReferenceDiffUrl  :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReference

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving (Show, Generic)

instance NFData EditPullRequestState
