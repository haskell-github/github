-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.PullRequests (
    SimplePullRequest(..),
    PullRequest(..),
    EditPullRequest(..),
    CreatePullRequest(..),
    PullRequestLinks(..),
    PullRequestCommit(..),
    PullRequestEvent(..),
    PullRequestEventType(..),
    PullRequestReference(..),
    MergeResult(..),
    ) where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Options     (IssueState (..), MergeableState (..))
import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)
import GitHub.Data.Teams       (SimpleTeam)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T

data SimplePullRequest = SimplePullRequest
    { simplePullRequestClosedAt           :: !(Maybe UTCTime)
    , simplePullRequestCreatedAt          :: !UTCTime
    , simplePullRequestUser               :: !SimpleUser
    , simplePullRequestPatchUrl           :: !URL
    , simplePullRequestState              :: !IssueState
    , simplePullRequestNumber             :: !IssueNumber
    , simplePullRequestHtmlUrl            :: !URL
    , simplePullRequestUpdatedAt          :: !UTCTime
    , simplePullRequestBody               :: !(Maybe Text)
    , simplePullRequestAssignees          :: (Vector SimpleUser)
    , simplePullRequestRequestedReviewers :: (Vector SimpleUser)
    , simplePullRequestRequestedTeamReviewers:: (Vector SimpleTeam)
    , simplePullRequestIssueUrl           :: !URL
    , simplePullRequestDiffUrl            :: !URL
    , simplePullRequestUrl                :: !URL
    , simplePullRequestLinks              :: !PullRequestLinks
    , simplePullRequestMergedAt           :: !(Maybe UTCTime)
    , simplePullRequestTitle              :: !Text
    , simplePullRequestId                 :: !(Id PullRequest)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimplePullRequest where rnf = genericRnf
instance Binary SimplePullRequest

data PullRequest = PullRequest
    { pullRequestClosedAt             :: !(Maybe UTCTime)
    , pullRequestCreatedAt            :: !UTCTime
    , pullRequestUser                 :: !SimpleUser
    , pullRequestPatchUrl             :: !URL
    , pullRequestState                :: !IssueState
    , pullRequestNumber               :: !IssueNumber
    , pullRequestHtmlUrl              :: !URL
    , pullRequestUpdatedAt            :: !UTCTime
    , pullRequestBody                 :: !(Maybe Text)
    , pullRequestAssignees            :: (Vector SimpleUser)
    , pullRequestRequestedReviewers   :: (Vector SimpleUser)
    , pullRequestRequestedTeamReviewers :: (Vector SimpleTeam)
    , pullRequestIssueUrl             :: !URL
    , pullRequestDiffUrl              :: !URL
    , pullRequestUrl                  :: !URL
    , pullRequestLinks                :: !PullRequestLinks
    , pullRequestMergedAt             :: !(Maybe UTCTime)
    , pullRequestTitle                :: !Text
    , pullRequestId                   :: !(Id PullRequest)
    , pullRequestMergedBy             :: !(Maybe SimpleUser)
    , pullRequestChangedFiles         :: !Int
    , pullRequestHead                 :: !PullRequestCommit
    , pullRequestComments             :: !Count
    , pullRequestDeletions            :: !Count
    , pullRequestAdditions            :: !Count
    , pullRequestReviewComments       :: !Count
    , pullRequestBase                 :: !PullRequestCommit
    , pullRequestCommits              :: !Count
    , pullRequestMerged               :: !Bool
    , pullRequestMergeable            :: !(Maybe Bool)
    , pullRequestMergeableState       :: !MergeableState
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest where rnf = genericRnf
instance Binary PullRequest

data EditPullRequest = EditPullRequest
    { editPullRequestTitle :: !(Maybe Text)
    , editPullRequestBody  :: !(Maybe Text)
    , editPullRequestState :: !(Maybe IssueState)
    , editPullRequestBase  :: !(Maybe Text)
    , editPullRequestMaintainerCanModify
                           :: !(Maybe Bool)
    }
  deriving (Show, Generic)

instance NFData EditPullRequest where rnf = genericRnf
instance Binary EditPullRequest

data CreatePullRequest
    = CreatePullRequest
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

data PullRequestLinks = PullRequestLinks
    { pullRequestLinksReviewComments :: !URL
    , pullRequestLinksComments       :: !URL
    , pullRequestLinksHtml           :: !URL
    , pullRequestLinksSelf           :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks where rnf = genericRnf
instance Binary PullRequestLinks

data PullRequestCommit = PullRequestCommit
    { pullRequestCommitLabel :: !Text
    , pullRequestCommitRef   :: !Text
    , pullRequestCommitSha   :: !Text
    , pullRequestCommitUser  :: !SimpleUser
    , pullRequestCommitRepo  :: !(Maybe Repo)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit where rnf = genericRnf
instance Binary PullRequestCommit

data PullRequestEvent = PullRequestEvent
    { pullRequestEventAction      :: !PullRequestEventType
    , pullRequestEventNumber      :: !Int
    , pullRequestEventPullRequest :: !PullRequest
    , pullRequestRepository       :: !Repo
    , pullRequestSender           :: !SimpleUser
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEvent where rnf = genericRnf
instance Binary PullRequestEvent

data PullRequestEventType
    = PullRequestOpened
    | PullRequestClosed
    | PullRequestSynchronized
    | PullRequestReopened
    | PullRequestAssigned
    | PullRequestUnassigned
    | PullRequestLabeled
    | PullRequestUnlabeled
    | PullRequestReviewRequested
    | PullRequestReviewRequestRemoved
    | PullRequestEdited
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEventType where rnf = genericRnf
instance Binary PullRequestEventType

data PullRequestReference = PullRequestReference
    { pullRequestReferenceHtmlUrl  :: !(Maybe URL)
    , pullRequestReferencePatchUrl :: !(Maybe URL)
    , pullRequestReferenceDiffUrl  :: !(Maybe URL)
    }
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestReference where rnf = genericRnf
instance Binary PullRequestReference


-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON SimplePullRequest where
    parseJSON = withObject "SimplePullRequest" $ \o -> SimplePullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .:? "body"
        <*> o .: "assignees"
        <*> o .:? "requested_reviewers" .!= mempty
        <*> o .:? "requested_teams" .!= mempty
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"

instance ToJSON EditPullRequest where
    toJSON (EditPullRequest t b s base mcm) =
        object $ filter notNull
            [ "title" .= t
            , "body"  .= b
            , "state" .= s
            , "base"  .= base
            , "maintainer_can_modify"
                      .= mcm
            ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

instance ToJSON CreatePullRequest where
    toJSON (CreatePullRequest t b headPR basePR) =
        object [ "title" .= t, "body" .= b, "head" .= headPR, "base" .= basePR ]
    toJSON (CreatePullRequestIssue issueNum headPR basePR) =
        object [ "issue" .= issueNum, "head" .= headPR, "base" .= basePR]

instance FromJSON PullRequest where
    parseJSON = withObject "PullRequest" $ \o -> PullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .:? "body"
        <*> o .: "assignees"
        <*> o .:? "requested_reviewers" .!= mempty
        <*> o .:? "requested_teams" .!= mempty
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
        <*> o .: "mergeable_state"

instance FromJSON PullRequestLinks where
    parseJSON = withObject "PullRequestLinks" $ \o -> PullRequestLinks
        <$> fmap getHref (o .: "review_comments")
        <*> fmap getHref (o .: "comments")
        <*> fmap getHref (o .: "html")
        <*> fmap getHref (o .: "self")

instance FromJSON PullRequestCommit where
    parseJSON = withObject "PullRequestCommit" $ \o -> PullRequestCommit
        <$> o .: "label"
        <*> o .: "ref"
        <*> o .: "sha"
        <*> o .: "user"
        <*> o .: "repo"

instance FromJSON PullRequestEvent where
    parseJSON = withObject "PullRequestEvent" $ \o -> PullRequestEvent
        <$> o .: "action"
        <*> o .: "number"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestEventType where
    parseJSON = withText "PullRequestEventType" $ \t -> case T.toLower t of
        "opened"                 -> pure PullRequestOpened
        "closed"                 -> pure PullRequestClosed
        "synchronize"            -> pure PullRequestSynchronized
        "reopened"               -> pure PullRequestReopened
        "assigned"               -> pure PullRequestAssigned
        "unassigned"             -> pure PullRequestUnassigned
        "labeled"                -> pure PullRequestLabeled
        "unlabeled"              -> pure PullRequestUnlabeled
        "review_requested"       -> pure PullRequestReviewRequested
        "review_request_removed" -> pure PullRequestReviewRequestRemoved
        "edited"                 -> pure PullRequestEdited
        _                        -> fail $ "Unknown PullRequestEventType: " <> T.unpack t

instance FromJSON PullRequestReference where
    parseJSON = withObject "PullRequestReference" $ \o -> PullRequestReference
        <$> o .:? "html_url"
        <*> o .:? "patch_url"
        <*> o .:? "diff_url"

-- Helpers

newtype Href a = Href { getHref :: a }

instance FromJSON a => FromJSON (Href a) where
    parseJSON = withObject "href object" $
        \obj -> Href <$> obj .: "href"

-- | Pull request merge results
data MergeResult
    = MergeSuccessful
    | MergeCannotPerform
    | MergeConflict
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)
