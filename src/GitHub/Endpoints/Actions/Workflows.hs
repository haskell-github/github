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
    :: Name Owner
    -> Name Repo
    -> Id Workflow
    -> GenRequest 'MtJSON 'RA  Workflow
workflowR user repo workflow  = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflow]
    []

-- | Disable a workflow.
-- See <https://docs.github.com/en/rest/actions/workflows#disable-a-workflow>
disableWorkflowR
    :: Name Owner
    -> Name Repo
    -> Id Workflow
    -> GenRequest 'MtUnit 'RW  ()
disableWorkflowR user repo workflow  = Command Put
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflow, "disable"]
    mempty

-- | Create a workflow dispatch event.
-- See <https://docs.github.com/en/rest/actions/workflows#create-a-workflow-dispatch-event>
triggerWorkflowR
    :: (ToJSON a) => Name Owner
    -> Name Repo
    -> Id Workflow
    -> CreateWorkflowDispatchEvent a
    -> GenRequest 'MtUnit 'RW  ()
triggerWorkflowR user repo workflow  = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflow, "dispatches"]
    . encode

-- | Enable a workflow.
-- See <https://docs.github.com/en/rest/actions/workflows#enable-a-workflow>
enableWorkflowR
    :: Name Owner
    -> Name Repo
    -> Id Workflow
    -> GenRequest 'MtUnit 'RW  ()
enableWorkflowR user repo workflow  = Command Put
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflow, "enable"]
    mempty
