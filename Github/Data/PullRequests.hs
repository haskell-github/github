{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.PullRequests where

import Github.Data.Definitions
import Github.Data.Repos       (Repo)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

data SimplePullRequest = SimplePullRequest {
   simplePullRequestClosedAt  :: !(Maybe UTCTime)
  ,simplePullRequestCreatedAt :: !UTCTime
  ,simplePullRequestUser      :: !SimpleOwner
  ,simplePullRequestPatchUrl  :: !Text
  ,simplePullRequestState     :: !Text
  ,simplePullRequestNumber    :: !Int
  ,simplePullRequestHtmlUrl   :: !Text
  ,simplePullRequestUpdatedAt :: !UTCTime
  ,simplePullRequestBody      :: !Text
  ,simplePullRequestIssueUrl  :: !Text
  ,simplePullRequestDiffUrl   :: !Text
  ,simplePullRequestUrl       :: !Text
  ,simplePullRequestLinks     :: !PullRequestLinks
  ,simplePullRequestMergedAt  :: !(Maybe UTCTime)
  ,simplePullRequestTitle     :: !Text
  ,simplePullRequestId        :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimplePullRequest where rnf = genericRnf
instance Binary SimplePullRequest

data PullRequest = PullRequest {
  -- this is a duplication of a PullRequest
   pullRequestClosedAt       :: !(Maybe UTCTime)
  ,pullRequestCreatedAt      :: !UTCTime
  ,pullRequestUser           :: !SimpleOwner
  ,pullRequestPatchUrl       :: !Text
  ,pullRequestState          :: !Text
  ,pullRequestNumber         :: !Int
  ,pullRequestHtmlUrl        :: !Text
  ,pullRequestUpdatedAt      :: !UTCTime
  ,pullRequestBody           :: !Text
  ,pullRequestIssueUrl       :: !Text
  ,pullRequestDiffUrl        :: !Text
  ,pullRequestUrl            :: !Text
  ,pullRequestLinks          :: !PullRequestLinks
  ,pullRequestMergedAt       :: !(Maybe UTCTime)
  ,pullRequestTitle          :: !Text
  ,pullRequestId             :: !Int
  ,pullRequestMergedBy       :: !(Maybe SimpleOwner)
  ,pullRequestChangedFiles   :: !Int
  ,pullRequestHead           :: !PullRequestCommit
  ,pullRequestComments       :: !Int
  ,pullRequestDeletions      :: !Int
  ,pullRequestAdditions      :: !Int
  ,pullRequestReviewComments :: !Int
  ,pullRequestBase           :: !PullRequestCommit
  ,pullRequestCommits        :: !Int
  ,pullRequestMerged         :: !Bool
  ,pullRequestMergeable      :: !(Maybe Bool)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest where rnf = genericRnf
instance Binary PullRequest

data EditPullRequest = EditPullRequest {
   editPullRequestTitle :: !(Maybe Text)
  ,editPullRequestBody  :: !(Maybe Text)
  ,editPullRequestState :: !(Maybe EditPullRequestState)
} deriving (Show, Generic)

instance NFData EditPullRequest where rnf = genericRnf
instance Binary EditPullRequest

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
instance Binary CreatePullRequest

data PullRequestLinks = PullRequestLinks {
   pullRequestLinksReviewComments :: !Text
  ,pullRequestLinksComments       :: !Text
  ,pullRequestLinksHtml           :: !Text
  ,pullRequestLinksSelf           :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks where rnf = genericRnf
instance Binary PullRequestLinks

data PullRequestCommit = PullRequestCommit {
   pullRequestCommitLabel :: !Text
  ,pullRequestCommitRef   :: !Text
  ,pullRequestCommitSha   :: !Text
  ,pullRequestCommitUser  :: !SimpleOwner
  ,pullRequestCommitRepo  :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit where rnf = genericRnf
instance Binary PullRequestCommit

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction      :: !PullRequestEventType
  ,pullRequestEventNumber      :: !Int
  ,pullRequestEventPullRequest :: !PullRequest
  ,pullRequestRepository       :: !Repo
  ,pullRequestSender           :: !SimpleOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEvent where rnf = genericRnf
instance Binary PullRequestEvent

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
instance Binary PullRequestEventType

data PullRequestReference = PullRequestReference {
  pullRequestReferenceHtmlUrl   :: !(Maybe Text)
  ,pullRequestReferencePatchUrl :: !(Maybe Text)
  ,pullRequestReferenceDiffUrl  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReference where rnf = genericRnf
instance Binary PullRequestReference

data EditPullRequestState =
    EditPullRequestStateOpen
  | EditPullRequestStateClosed
  deriving (Show, Generic)

instance NFData EditPullRequestState where rnf = genericRnf
instance Binary EditPullRequestState
