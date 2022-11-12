{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Data.Actions.Workflows (
    Workflow(..),
    -- ActionWorkflowResult(..),
    -- ActionWorkflowRun(..),
    -- Workflow,
    -- ActionWorkflowRunResult(..),
    CreateWorkflowDispatchEvent(..),
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

data Workflow = Workflow
    { 
     workflowWorkflowId                 :: !(Id Workflow)
    , workflowName                 :: !Text
    , workflowPath                 :: !Text
    , workflowState                 :: !Text
    , workflowCreatedAt                 :: !UTCTime
    , workflowUpdatedAt                 :: !UTCTime
    , workflowUrl                 :: !UTCTime
    , workflowHtmlUrl                 :: !UTCTime
    , workflowBadgeUrl                 :: !UTCTime
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

data CreateWorkflowDispatchEvent a
    = CreateWorkflowDispatchEvent
      { createWorkflowDispatchEventRef :: !Text
      , createWorkflowDispatchEventInputs  :: !a
      }
  deriving (Show, Generic)

instance (NFData a) => NFData (CreateWorkflowDispatchEvent a) where rnf = genericRnf
instance (Binary a) => Binary (CreateWorkflowDispatchEvent a)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON Workflow where
    parseJSON = withObject "Workflow" $ \o -> Workflow
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "path"
        <*> o .: "state"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "badge_url"

instance FromJSON (WithTotalCount Workflow) where
    parseJSON = withObject "WorkflowList" $ \o -> WithTotalCount
        <$> o .: "workflows"
        <*> o .: "total_count"

-- instance FromJSON a => FromJSON (ActionWorkflowResult a) where
--     parseJSON = withObject "ActionWorkflowResult" $ \o -> ActionWorkflowResult
--         <$> o .: "total_count"
--         <*> o .:? "workflows" .!= V.empty

-- instance FromJSON a => FromJSON (ActionWorkflowRunResult a) where
--     parseJSON = withObject "ActionWorkflowRunResult" $ \o -> ActionWorkflowRunResult
--         <$> o .: "total_count"
--         <*> o .:? "workflow_runs" .!= V.empty

-- instance FromJSON RunCommit where
--     parseJSON = withObject "RunCommit" $ \o -> RunCommit
--         <$> o .: "id"
--         <*> o .: "tree_id"

-- instance FromJSON ActionWorkflowRun where
--     parseJSON = withObject "ActionWorkflowRun" $ \o -> ActionWorkflowRun
--         <$> o .: "id"
--         <*> o .: "head_branch"
--         <*> o .: "head_sha"
--         <*> o .: "status"
--         <*> o .: "url"
--         <*> o .: "html_url"
--         <*> o .: "created_at"
--         <*> o .: "updated_at"
--         -- <*> o .: "repository"
--         <*> o .: "head_commit"
--         <*> o .:? "conclusion"


instance ToJSON a => ToJSON (CreateWorkflowDispatchEvent a) where
    toJSON (CreateWorkflowDispatchEvent ref inputs) =
        object [ "ref" .= ref, "inputs" .= inputs ]