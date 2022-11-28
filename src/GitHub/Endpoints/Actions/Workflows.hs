-- -----------------------------------------------------------------------------
-- -- |
-- -- License     :  BSD-3-Clause
-- -- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- --
-- -- The pull requests API as documented at
-- -- <http://developer.github.com/v3/pulls/>.
module GitHub.Endpoints.Actions.Workflows (
    repositoryWorkflowsR,
    workflowR,
    disableWorkflowR,
    triggerWorkflowR,
    enableWorkflowR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List repository workflows.
-- See <https://docs.github.com/en/rest/actions/workflows#list-repository-workflows>
repositoryWorkflowsR
    :: Name Owner
    -> Name Repo
    -> FetchCount
    -> GenRequest 'MtJSON 'RA (WithTotalCount Workflow)
repositoryWorkflowsR user repo  = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows"]
    []

-- | Get a workflow.
-- See <https://docs.github.com/en/rest/actions/workflows#get-a-workflow>
workflowR
    :: (IsPathPart idOrName) => Name Owner
    -> Name Repo
    -> idOrName
    -> GenRequest 'MtJSON 'RA  Workflow
workflowR user repo idOrName  = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart idOrName]
    []

-- | Disable a workflow.
-- See <https://docs.github.com/en/rest/actions/workflows#disable-a-workflow>
disableWorkflowR
    :: (IsPathPart idOrName) => Name Owner
    -> Name Repo
    -> idOrName
    -> GenRequest 'MtUnit 'RW  ()
disableWorkflowR user repo workflow  = Command Put
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart idOrName, "disable"]
    mempty

-- | Create a workflow dispatch event.
-- See <https://docs.github.com/en/rest/actions/workflows#create-a-workflow-dispatch-event>
triggerWorkflowR
    :: (ToJSON a, IsPathPart idOrName) => Name Owner
    -> Name Repo
    -> idOrName
    -> CreateWorkflowDispatchEvent a
    -> GenRequest 'MtUnit 'RW  ()
triggerWorkflowR user repo workflow  = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart idOrName, "dispatches"]
    . encode

-- | Enable a workflow.
-- See <https://docs.github.com/en/rest/actions/workflows#enable-a-workflow>
enableWorkflowR
    :: (IsPathPart idOrName) => Name Owner
    -> Name Repo
    -> idOrName
    -> GenRequest 'MtUnit 'RW  ()
enableWorkflowR user repo workflow  = Command Put
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart idOrName, "enable"]
    mempty
