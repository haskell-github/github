{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module GitHub.Data.Actions.WorkflowRuns (
    WorkflowRun(..),
    RunAttempt(..),
    ReviewHistory(..),
    ) where

import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Data.Definitions
import GitHub.Data.PullRequests   (SimplePullRequest)
import GitHub.Data.URL            (URL)
import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Data.Id   (Id)
import GitHub.Data.Name (Name)

data WorkflowRun  = WorkflowRun
    { workflowRunWorkflowRunId :: !(Id WorkflowRun)
    , workflowRunName :: !(Name WorkflowRun)
    , workflowRunHeadBranch :: !Text
    , workflowRunHeadSha :: !Text
    , workflowRunPath :: !Text
    , workflowRunDisplayTitle :: !Text
    , workflowRunRunNumber :: !Integer
    , workflowRunEvent :: !Text
    , workflowRunStatus :: !Text
    , workflowRunConclusion :: !Text
    , workflowRunWorkflowId :: !Integer
    , workflowRunUrl :: !URL
    , workflowRunHtmlUrl :: !URL
    , workflowRunPullRequests :: !(Vector SimplePullRequest)
    , workflowRunCreatedAt :: !UTCTime
    , workflowRunUpdatedAt :: !UTCTime
    , workflowRunActor :: !SimpleUser
    , workflowRunAttempt :: !Integer
    , workflowRunStartedAt :: !UTCTime
    , workflowRunTrigerringActor :: !SimpleUser
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data RunAttempt = RunAttempt
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data ReviewHistory  = ReviewHistory
    { reviewHistoryState :: !Text
    , reviewHistoryComment :: !Text
    , reviewHistoryUser :: !SimpleUser

    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON WorkflowRun where
    parseJSON = withObject "WorkflowRun" $ \o -> WorkflowRun
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "head_branch"
        <*> o .: "head_sha"
        <*> o .: "path"
        <*> o .: "display_title"
        <*> o .: "run_number"
        <*> o .: "event"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "workflow_id"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "pull_requests"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "actor"
        <*> o .: "run_attempt"
        <*> o .: "run_started_at"
        <*> o .: "triggering_actor"

instance FromJSON (WithTotalCount WorkflowRun) where
    parseJSON = withObject "WorkflowRunList" $ \o -> WithTotalCount
        <$> o .: "workflow_runs"
        <*> o .: "total_count"

instance FromJSON ReviewHistory where
    parseJSON = withObject "ReviewHistory" $ \o -> ReviewHistory
        <$> o .: "state"
        <*> o .: "comment"
        <*> o .: "user"
