{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module GitHub.Data.Actions.Workflows (
    Workflow(..),
    CreateWorkflowDispatchEvent(..),
    ) where

import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Data.Id             (Id)
import GitHub.Data.URL            (URL)
import GitHub.Internal.Prelude
import Prelude ()


data Workflow = Workflow
    {
     workflowWorkflowId                 :: !(Id Workflow)
    , workflowName                 :: !Text
    , workflowPath                 :: !Text
    , workflowState                 :: !Text
    , workflowCreatedAt                 :: !UTCTime
    , workflowUpdatedAt                 :: !UTCTime
    , workflowUrl                 :: !URL
    , workflowHtmlUrl                 :: !URL
    , workflowBadgeUrl                 :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

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

instance ToJSON a => ToJSON (CreateWorkflowDispatchEvent a) where
    toJSON (CreateWorkflowDispatchEvent ref inputs) =
        object [ "ref" .= ref, "inputs" .= inputs ]
