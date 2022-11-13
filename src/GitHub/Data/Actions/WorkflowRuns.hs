{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Data.Actions.WorkflowRuns (
    -- Workflow(..),
    -- ActionWorkflowResult(..),
    -- ActionWorkflowRun(..),
    -- Workflow,
    -- ActionWorkflowRunResult(..),
    WorkflowRun(..),
    RunAttempt(..),
    ReviewHistory(..),
    ) where

import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Data.Definitions
import GitHub.Data.Id             (Id)
import GitHub.Data.Options        (IssueState (..), MergeableState (..))
import GitHub.Data.Repos          (Repo)
import GitHub.Data.URL            (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text                as T
import qualified Data.Vector              as V
import           GitHub.Data.PullRequests (SimplePullRequest)

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
    , workflowRunRepository :: !Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data RunAttempt = RunAttempt
    -- {
    --  jobId                 :: !(Id Job)
    -- , runRunId                 :: !(Id WorkflowRun)
    -- , workflowPath                 :: !Text
    -- , workflowState                 :: !Text
    -- , workflowCreatedAt                 :: !UTCTime
    -- , workflowUpdatedAt                 :: !UTCTime
    -- , workflowUrl                 :: !UTCTime
    -- , workflowHtmlUrl                 :: !UTCTime
    -- , workflowBadgeUrl                 :: !UTCTime
    -- }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data ReviewHistory  = ReviewHistory
    { reviewHistoryState :: !Text
    , reviewHistoryComment :: !Text
    , reviewHistoryUser :: !SimpleUser

    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)
-- data RunCommit = RunCommit
--     {
--      runCommitId   :: !Text
--     , runCommitTreeId   :: !Text
--     }
--   deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- instance NFData RunCommit where rnf = genericRnf
-- instance Binary RunCommit


-- data ActionWorkflowRun = ActionWorkflowRun
--     {
--         actionWorkflowRunId :: !(Id ActionWorkflowRun)
--     ,  actionWorkflowRunHeadBranch :: !Text
--     ,  actionWorkflowRunHeadSha :: !Text
--     , actionWorkflowRunStatus :: !Text
--     , actionWorkflowRunUrl :: !URL
--     , actionWorkflowRunHtmlUrl :: !URL
--     , actionWorkflowRunCreatedAt :: !UTCTime
--     , actionWorkflowRunUpdatedAt :: !UTCTime
--     -- , actionWorkflowRunRepo :: !Repo
--     , actionWorkflowRunHeadCommit :: !RunCommit
--     , actionWorkflowRunConclusion :: !(Maybe Text)
--     } deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- data ActionWorkflowResult entity = ActionWorkflowResult
--     {
--          actionWorkflowTotalCount :: !Int
--         , actionWorkflowResults    :: !(Vector entity)
--     } deriving (Show, Data, Typeable, Eq, Ord, Generic)


-- data ActionWorkflowRunResult entity = ActionWorkflowRunResult
--     {
--          actionWorkflowRunResultTotalCount :: !Int
--         , actionWorkflowRunResultResults   :: !(Vector entity)
--     } deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- data CreateWorkflowDispatchEvent a
--     = CreateWorkflowDispatchEvent
--       { createWorkflowDispatchEventRef :: !Text
--       , createWorkflowDispatchEventInputs  :: !a
--       }
--   deriving (Show, Generic)

-- instance (NFData a) => NFData (CreateWorkflowDispatchEvent a) where rnf = genericRnf
-- instance (Binary a) => Binary (CreateWorkflowDispatchEvent a)

-- -------------------------------------------------------------------------------
-- -- JSON instances
-- -------------------------------------------------------------------------------

-- instance FromJSON Workflow where
--     parseJSON = withObject "Workflow" $ \o -> Workflow
--         <$> o .: "id"
--         <*> o .: "name"
--         <*> o .: "path"
--         <*> o .: "state"
--         <*> o .: "created_at"
--         <*> o .: "updated_at"
--         <*> o .: "url"
--         <*> o .: "html_url"
--         <*> o .: "badge_url"

-- instance FromJSON (WithTotalCount Workflow) where
--     parseJSON = withObject "WorkflowList" $ \o -> WithTotalCount
--         <$> o .: "workflows"
--         <*> o .: "total_count"

-- -- instance FromJSON a => FromJSON (ActionWorkflowResult a) where
-- --     parseJSON = withObject "ActionWorkflowResult" $ \o -> ActionWorkflowResult
-- --         <$> o .: "total_count"
-- --         <*> o .:? "workflows" .!= V.empty

-- -- instance FromJSON a => FromJSON (ActionWorkflowRunResult a) where
-- --     parseJSON = withObject "ActionWorkflowRunResult" $ \o -> ActionWorkflowRunResult
-- --         <$> o .: "total_count"
-- --         <*> o .:? "workflow_runs" .!= V.empty

-- -- instance FromJSON RunCommit where
-- --     parseJSON = withObject "RunCommit" $ \o -> RunCommit
-- --         <$> o .: "id"
-- --         <*> o .: "tree_id"

-- -- instance FromJSON ActionWorkflowRun where
-- --     parseJSON = withObject "ActionWorkflowRun" $ \o -> ActionWorkflowRun
-- --         <$> o .: "id"
-- --         <*> o .: "head_branch"
-- --         <*> o .: "head_sha"
-- --         <*> o .: "status"
-- --         <*> o .: "url"
-- --         <*> o .: "html_url"
-- --         <*> o .: "created_at"
-- --         <*> o .: "updated_at"
-- --         -- <*> o .: "repository"
-- --         <*> o .: "head_commit"
-- --         <*> o .:? "conclusion"


-- instance ToJSON a => ToJSON (CreateWorkflowDispatchEvent a) where
--     toJSON (CreateWorkflowDispatchEvent ref inputs) =
--         object [ "ref" .= ref, "inputs" .= inputs ]

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
        <*> o .: "attempt"
        <*> o .: "started_at"
        <*> o .: "trigerring_actor"
        <*> o .: "repository"
