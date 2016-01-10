{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.PullRequests where

import Github.Data.Definitions
import Github.Data.Repos       (Repo)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import Data.Time       (UTCTime)
import GHC.Generics    (Generic)

data PullRequest = PullRequest {
   pullRequestClosedAt  :: !(Maybe UTCTime)
  ,pullRequestCreatedAt :: !UTCTime
  ,pullRequestUser      :: !GithubOwner
  ,pullRequestPatchUrl  :: !Text
  ,pullRequestState     :: !Text
  ,pullRequestNumber    :: !Int
  ,pullRequestHtmlUrl   :: !Text
  ,pullRequestUpdatedAt :: !UTCTime
  ,pullRequestBody      :: !Text
  ,pullRequestIssueUrl  :: !Text
  ,pullRequestDiffUrl   :: !Text
  ,pullRequestUrl       :: !Text
  ,pullRequestLinks     :: !PullRequestLinks
  ,pullRequestMergedAt  :: !(Maybe UTCTime)
  ,pullRequestTitle     :: !Text
  ,pullRequestId        :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest where rnf = genericRnf

data DetailedPullRequest = DetailedPullRequest {
  -- this is a duplication of a PullRequest
   detailedPullRequestClosedAt       :: !(Maybe UTCTime)
  ,detailedPullRequestCreatedAt      :: !UTCTime
  ,detailedPullRequestUser           :: !GithubOwner
  ,detailedPullRequestPatchUrl       :: !Text
  ,detailedPullRequestState          :: !Text
  ,detailedPullRequestNumber         :: !Int
  ,detailedPullRequestHtmlUrl        :: !Text
  ,detailedPullRequestUpdatedAt      :: !UTCTime
  ,detailedPullRequestBody           :: !Text
  ,detailedPullRequestIssueUrl       :: !Text
  ,detailedPullRequestDiffUrl        :: !Text
  ,detailedPullRequestUrl            :: !Text
  ,detailedPullRequestLinks          :: !PullRequestLinks
  ,detailedPullRequestMergedAt       :: !(Maybe UTCTime)
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

instance NFData DetailedPullRequest where rnf = genericRnf

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: !(Maybe Text)
  ,editPullRequestBody  :: !(Maybe Text)
  ,editPullRequestState :: !(Maybe EditPullRequestState)
} deriving (Show, Generic)

instance NFData EditPullRequest where rnf = genericRnf

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

instance NFData CreatePullRequest where rnf = genericRnf

data PullRequestLinks = PullRequestLinks {
   pullRequestLinksReviewComments :: !Text
  ,pullRequestLinksComments       :: !Text
  ,pullRequestLinksHtml           :: !Text
  ,pullRequestLinksSelf           :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks where rnf = genericRnf

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: !Text
  ,pullRequestCommitRef   :: !Text
  ,pullRequestCommitSha   :: !Text
  ,pullRequestCommitUser  :: !GithubOwner
  ,pullRequestCommitRepo  :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit where rnf = genericRnf

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction      :: !PullRequestEventType
  ,pullRequestEventNumber      :: !Int
  ,pullRequestEventPullRequest :: !DetailedPullRequest
  ,pullRequestRepository       :: !Repo
  ,pullRequestSender           :: !GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEvent where rnf = genericRnf

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

instance NFData PullRequestEventType where rnf = genericRnf

data PullRequestReference = PullRequestReference {
  pullRequestReferenceHtmlUrl   :: !(Maybe Text)
  ,pullRequestReferencePatchUrl :: !(Maybe Text)
  ,pullRequestReferenceDiffUrl  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReference where rnf = genericRnf

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving (Show, Generic)

instance NFData EditPullRequestState where rnf = genericRnf
