-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module GitHub.Data.Actions.Artifacts (
    Artifact(..),
    ArtifactWorkflowRun(..),
    ) where

import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Data.Actions.Common       (WithTotalCount (WithTotalCount))
import GitHub.Data.Actions.WorkflowRuns (WorkflowRun)
import GitHub.Data.Repos                (Repo)


-------------------------------------------------------------------------------
-- Artifact
-------------------------------------------------------------------------------
data ArtifactWorkflowRun  = ArtifactWorkflowRun
    { artifactWorkflowRunWorkflowRunId :: !(Id WorkflowRun)
    , artifactWorkflowRunRepositoryId :: !(Id Repo)
    , artifactWorkflowRunHeadRepositoryId :: !(Id Repo)
    , artifactWorkflowRunHeadBranch :: !Text
    , artifactWorkflowRunHeadSha :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data Artifact = Artifact
    { artifactArchiveDownloadUrl :: !URL
    , artifactCreatedAt :: !UTCTime
    , artifactExpired :: !Bool
    , artifactExpiresAt :: !UTCTime
    , artifactId :: !(Id Artifact)
    , artifactName :: !Text
    , artifactNodeId :: !Text
    , artifactSizeInBytes :: !Int
    , artifactUpdatedAt :: !UTCTime
    , artifactUrl :: !URL
    , artifactWorkflowRun :: !ArtifactWorkflowRun
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON ArtifactWorkflowRun where
    parseJSON = withObject "ArtifactWorkflowRun" $ \o -> ArtifactWorkflowRun
        <$> o .: "id"
        <*> o .: "repository_id"
        <*> o .: "head_repository_id"
        <*> o .: "head_branch"
        <*> o .: "head_sha"

instance FromJSON Artifact where
    parseJSON = withObject "Artifact" $ \o -> Artifact
        <$> o .: "archive_download_url"
        <*> o .: "created_at"
        <*> o .: "expired"
        <*> o .: "expires_at"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "node_id"
        <*> o .: "size_in_bytes"
        <*> o .: "updated_at"
        <*> o .: "url"
        <*> o .: "workflow_run"

instance FromJSON (WithTotalCount Artifact) where
    parseJSON = withObject "ArtifactList" $ \o -> WithTotalCount
        <$> o .: "artifacts"
        <*> o .: "total_count"
