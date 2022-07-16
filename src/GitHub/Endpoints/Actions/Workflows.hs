-- -----------------------------------------------------------------------------
-- -- |
-- -- License     :  BSD-3-Clause
-- -- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- --
-- -- The pull requests API as documented at
-- -- <http://developer.github.com/v3/pulls/>.
-- module GitHub.Endpoints.Actions.Workflows (
--     workflowsForR,
--     workflowRunsForR,
--     createWorkflowDispatchEventR,
--     workflowRunForR,
--     module GitHub.Data
--     ) where

-- import GitHub.Data
-- import GitHub.Internal.Prelude
-- import Prelude ()
-- import GitHub.Data.Actions (ActionWorkflow, ActionWorkflowResult, ActionWorkflowRun, Workflow, ActionWorkflowRunResult, CreateWorkflowDispatchEvent)

-- -- | List pull requests.
-- -- See <TODO>
-- workflowsForR
--     :: Name Owner
--     -> Name Repo
--     -- -> FetchCount
--     -> Request k (ActionWorkflowResult ActionWorkflow)
-- workflowsForR user repo  = query
--     ["repos", toPathPart user, toPathPart repo, "actions", "workflows"]
--     []

-- -- TODO move?
-- -- data Workflow


-- workflowRunsForR
--     :: Name Owner
--     -> Name Repo
--     -> Name Workflow
--     -- -> FetchCount
--     -> Request k (ActionWorkflowRunResult ActionWorkflowRun)
-- workflowRunsForR user repo workflowId = query
--     ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflowId, "runs"]
--     []

-- -- | Create a pull request.
-- -- See <TODO>
-- createWorkflowDispatchEventR :: (ToJSON a) => Name Owner
--                    -> Name Repo
--                    -> Name Workflow
--                    -> CreateWorkflowDispatchEvent a
--                    -> GenRequest 'MtUnit 'RW ()
-- createWorkflowDispatchEventR user repo workflowId cwde =
--     Command Post ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflowId, "dispatches"] (encode cwde)


-- workflowRunForR
--     :: Name Owner
--     -> Name Repo
--     -> Id ActionWorkflowRun 
--     -> Request k (ActionWorkflowRun)
-- workflowRunForR user repo runId = query
--     ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart runId]
--     []