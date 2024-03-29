-- |
-- The actions API as documented at
-- <https://docs.github.com/en/rest/reference/actions>.

module GitHub.Endpoints.Actions.Artifacts (
    artifactsForR,
    artifactR,
    deleteArtifactR,
    downloadArtifactR,
    artifactsForWorkflowRunR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Network.URI             (URI)
import Prelude ()

-- | List artifacts for repository.
-- See <https://docs.github.com/en/rest/reference/actions#list-artifacts-for-a-repository>
artifactsForR
    :: Name Owner
    -> Name Repo
    -> ArtifactMod
    -> FetchCount
    -> Request 'RA (WithTotalCount Artifact)
artifactsForR user repo opts = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "artifacts"]
    (artifactModToQueryString opts)

-- | Get an artifact.
-- See <https://docs.github.com/en/rest/reference/actions#get-an-artifact>
artifactR :: Name Owner -> Name Repo -> Id Artifact -> Request 'RA Artifact
artifactR user repo artid =
    query ["repos", toPathPart user, toPathPart repo, "actions", "artifacts", toPathPart artid] []

-- | Delete an artifact.
-- See <https://docs.github.com/en/rest/reference/actions#delete-an-artifact>
deleteArtifactR :: Name Owner -> Name Repo -> Id Comment -> GenRequest 'MtUnit 'RW ()
deleteArtifactR user repo artid =
    Command Delete parts mempty
  where
    parts = ["repos", toPathPart user, toPathPart repo, "actions", "artifacts", toPathPart artid]

-- | Download an artifact.
-- See <https://docs.github.com/en/rest/reference/actions#download-an-artifact>
downloadArtifactR :: Name Owner -> Name Repo -> Id Artifact -> GenRequest 'MtRedirect 'RW URI
downloadArtifactR user repo artid =
    Query ["repos", toPathPart user, toPathPart repo, "actions", "artifacts", toPathPart artid, "zip"] []

-- | List artifacts for a workflow run.
-- See <https://docs.github.com/en/rest/reference/actions#list-workflow-run-artifacts>
artifactsForWorkflowRunR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    -> FetchCount
    -> Request 'RA (WithTotalCount Artifact)
artifactsForWorkflowRunR user repo runid  = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart runid, "artifacts"]
    []
