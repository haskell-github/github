{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.PullRequests where

import Prelude        ()
import Prelude.Compat

import GitHub.Data.Definitions
import GitHub.Data.Repos       (Repo)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), ToJSON (..), Value (..), object,
                                 withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types         (Object, Parser)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

data SimplePullRequest = SimplePullRequest {
   simplePullRequestClosedAt  :: !(Maybe UTCTime)
  ,simplePullRequestCreatedAt :: !UTCTime
  ,simplePullRequestUser      :: !SimpleUser
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
  ,pullRequestUser           :: !SimpleUser
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
  ,pullRequestMergedBy       :: !(Maybe SimpleUser)
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
  ,pullRequestCommitUser  :: !SimpleUser
  ,pullRequestCommitRepo  :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit where rnf = genericRnf
instance Binary PullRequestCommit

data PullRequestEvent = PullRequestEvent {
   pullRequestEventAction      :: !PullRequestEventType
  ,pullRequestEventNumber      :: !Int
  ,pullRequestEventPullRequest :: !PullRequest
  ,pullRequestRepository       :: !Repo
  ,pullRequestSender           :: !SimpleUser
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

-- JSON instances


instance FromJSON SimplePullRequest where
  parseJSON = withObject "SimplePullRequest" $ \o ->
      SimplePullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .:? "body" .!= "" -- TODO: no body is treated as empty
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"

instance ToJSON EditPullRequestState where
  toJSON (EditPullRequestStateOpen) = String "open"
  toJSON (EditPullRequestStateClosed) = String "closed"

instance ToJSON EditPullRequest where
  toJSON (EditPullRequest t b s) =
    object $ filter notNull [ "title" .= t, "body" .= b, "state" .= s ]
    where notNull (_, Null) = False
          notNull (_, _) = True

instance ToJSON CreatePullRequest where
  toJSON (CreatePullRequest t b headPR basePR) =
    object [ "title" .= t, "body" .= b, "head" .= headPR, "base" .= basePR ]
  toJSON (CreatePullRequestIssue issueNum headPR basePR) =
    object [ "issue" .= issueNum, "head" .= headPR, "base" .= basePR]

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o ->
      PullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
        <*> o .:? "merged_by"
        <*> o .: "changed_files"
        <*> o .: "head"
        <*> o .: "comments"
        <*> o .: "deletions"
        <*> o .: "additions"
        <*> o .: "review_comments"
        <*> o .: "base"
        <*> o .: "commits"
        <*> o .: "merged"
        <*> o .:? "mergeable"

instance FromJSON PullRequestLinks where
  parseJSON = withObject "PullRequestLinks" $ \o ->
    PullRequestLinks <$> o <.:> ["review_comments", "href"]
                     <*> o <.:> ["comments", "href"]
                     <*> o <.:> ["html", "href"]
                     <*> o <.:> ["self", "href"]

instance FromJSON PullRequestCommit where
  parseJSON = withObject "PullRequestCommit" $ \o ->
    PullRequestCommit <$> o .: "label"
                      <*> o .: "ref"
                      <*> o .: "sha"
                      <*> o .: "user"
                      <*> o .: "repo"

instance FromJSON PullRequestEvent where
  parseJSON = withObject "PullRequestEvent" $ \o ->
    PullRequestEvent <$> o .: "action"
                     <*> o .: "number"
                     <*> o .: "pull_request"
                     <*> o .: "repository"
                     <*> o .: "sender"

instance FromJSON PullRequestEventType where
  parseJSON (String "opened") = pure PullRequestOpened
  parseJSON (String "closed") = pure PullRequestClosed
  parseJSON (String "synchronize") = pure PullRequestSynchronized
  parseJSON (String "reopened") = pure PullRequestReopened
  parseJSON (String "assigned") = pure PullRequestAssigned
  parseJSON (String "unassigned") = pure PullRequestUnassigned
  parseJSON (String "labeled") = pure PullRequestLabeled
  parseJSON (String "unlabeled") = pure PullRequestUnlabeled
  parseJSON _ = fail "Could not build a PullRequestEventType"

instance FromJSON PullRequestReference where
  parseJSON = withObject "PullRequestReference" $ \o ->
    PullRequestReference <$> o .:? "html_url"
                         <*> o .:? "patch_url"
                         <*> o .:? "diff_url"

-- Helpers

-- | Produce the value for the last key by traversing.
(<.:>) :: FromJSON v => Object -> [Text] -> Parser v
obj  <.:> [key]      = obj .: key
obj  <.:> (key:keys) = do
    obj' <- obj .: key
    obj' <.:> keys
_obj <.:> []         = fail "<.:> never happens - empty path"
