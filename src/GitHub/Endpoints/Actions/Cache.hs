-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The actions API as documented at
-- <https://docs.github.com/en/rest/reference/actions>.
module GitHub.Endpoints.Actions.Cache (
    cachesForRepoR,
    deleteCacheR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Network.URI             (URI)
import Prelude ()
-- import GitHub.Data.Actions (ActionWorkflow, ActionWorkflowResult, ActionWorkflowRun, Workflow, ActionWorkflowRunResult, CreateWorkflowDispatchEvent)

-- -- | List artifacts for repository.
-- -- See <https://docs.github.com/en/rest/reference/actions#list-artifacts-for-a-repository>
-- artifactsForR
--     :: Name Owner
--     -> Name Repo
--     -> FetchCount
--     -> Request k (WithTotalCount Artifact)
-- artifactsForR user repo  = PagedQuery
--     ["repos", toPathPart user, toPathPart repo, "actions", "artifacts"]
--     []


-- -- | Query a single artifact.
-- -- See <https://docs.github.com/en/rest/reference/actions#get-an-artifact>
-- artifactR :: Name Owner -> Name Repo -> Id Artifact -> Request k Artifact
-- artifactR user repo artid =
--     query ["repos", toPathPart user, toPathPart repo, "actions", "artifacts", toPathPart artid] []


-- -- | Download an artifact.
-- -- See <https://docs.github.com/en/rest/reference/actions#download-an-artifact>
-- downloadArtifactR :: Name Owner -> Name Repo -> Id Artifact -> GenRequest 'MtRedirect 'RW URI
-- downloadArtifactR user repo artid =
--     Query ["repos", toPathPart user, toPathPart repo, "actions", "artifacts", toPathPart artid, "zip"] []

-- | List the GitHub Actions caches for a repository.
-- See <https://docs.github.com/en/rest/actions/cache#list-github-actions-caches-for-a-repository>
cachesForRepoR
    :: Name Owner
    -> Name Repo
    -> CacheMod
    -> FetchCount
    -> GenRequest 'MtJSON 'RA (WithTotalCount Cache) 
cachesForRepoR user repo opts = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "caches"]
    (cacheModToQueryString opts)

-- | Delete GitHub Actions cache for a repository.
-- See <https://docs.github.com/en/rest/actions/cache#delete-github-actions-caches-for-a-repository-using-a-cache-key>
-- TODO: No querystring for Commands???
-- TODO: return value
-- deleteCachesKeyR :: Name Owner -> Name Repo -> String -> Maybe String -> GenRequest 'MtUnit 'RW ()
-- deleteCachesKeyR user repo key ref =
--     Command Delete parts mempty
--   where
--     parts = ["repos", toPathPart user, toPathPart repo, "actions", "caches"]

-- | Delete GitHub Actions cache for a repository.
-- See <https://docs.github.com/en/rest/actions/cache#delete-a-github-actions-cache-for-a-repository-using-a-cache-id>
deleteCacheR :: Name Owner -> Name Repo -> Id Cache -> GenRequest 'MtUnit 'RW ()
deleteCacheR user repo cacheid =
    Command Delete parts mempty
  where
    parts = ["repos", toPathPart user, toPathPart repo, "actions", "caches", toPathPart cacheid]