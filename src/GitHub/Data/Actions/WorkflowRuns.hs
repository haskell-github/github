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
    ) where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Options     (IssueState (..), MergeableState (..))
import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)
import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T
import qualified Data.Vector as V
    -- "run_id": 3353449941,
    -- "run_url": "https://api.github.com/repos/kote-test-org-actions/actions-api/actions/runs/3353449941",
    -- "run_attempt": 1,
    -- "node_id": "CR_kwDOIVc8sc8AAAACI12rNA",
    -- "head_sha": "3156f684232a3adec5085c920d2006aca80f2798",
    -- "url": "https://api.github.com/repos/kote-test-org-actions/actions-api/actions/jobs/9183275828",
    -- "html_url": "https://github.com/kote-test-org-actions/actions-api/actions/runs/3353449941/jobs/5556228789",
    -- "status": "completed",
    -- "conclusion": "success",
    -- "started_at": "2022-10-30T00:09:29Z",
    -- "completed_at": "2022-10-30T00:09:49Z",
    -- -- "name": "check-bats-version",
    --       {
    --     "name": "Set up job",
    --     "status": "completed",
    --     "conclusion": "success",
    --     "number": 1,
    --     "started_at": "2022-10-29T17:09:29.000-07:00",
    --     "completed_at": "2022-10-29T17:09:32.000-07:00"
    --   },
    --       "check_run_url": "https://api.github.com/repos/kote-test-org-actions/actions-api/check-runs/9183275828",
    -- "labels": [
    --   "ubuntu-latest"
    -- ],
    -- "runner_id": 1,
    -- "runner_name": "Hosted Agent",
    -- "runner_group_id": 2,
    -- "runner_group_name": "GitHub Actions"
data WorkflowRun  = WorkflowRun
    { workflowRunWorkflowRunId :: !(Id WorkflowRun)
    , workflowRunRepositoryId :: !(Id Repo)
    , workflowRunHeadRepositoryId :: !(Id Repo)
    , workflowRunHeadBranch :: !Text
    , workflowRunHeadSha :: !Text
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
        <*> o .: "repository_id"
        <*> o .: "head_repository_id"
        <*> o .: "head_branch"
        <*> o .: "head_sha"