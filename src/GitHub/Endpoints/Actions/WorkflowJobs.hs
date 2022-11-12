-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The actions API as documented at
-- <https://docs.github.com/en/rest/reference/actions>.
module GitHub.Endpoints.Actions.WorkflowJobs (
    jobR,
    downloadJobLogsR,
    jobsForWorkflowRunAttemptR,
    jobsForWorkflowRunR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()
import Network.URI (URI)

-- | Get a job for a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-jobs#get-a-job-for-a-workflow-run>
jobR :: Name Owner -> Name Repo -> Id Job -> Request 'RO Job
jobR owner repo job =
    Query ["repos", toPathPart owner, toPathPart repo, "actions", "jobs", toPathPart job] []

-- | Download job logs for a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-jobs#download-job-logs-for-a-workflow-run>
downloadJobLogsR :: Name Owner -> Name Repo -> Id Job -> GenRequest 'MtRedirect 'RO URI
downloadJobLogsR owner repo job =
    Query ["repos", toPathPart owner, toPathPart repo, "actions", "jobs", toPathPart job, "logs"] []

-- | List jobs for a workflow run attempt.
-- See <https://docs.github.com/en/rest/actions/workflow-jobs#list-jobs-for-a-workflow-run-attempt>
jobsForWorkflowRunAttemptR :: Name Owner -> Name Repo -> Id WorkflowRun -> Id RunAttempt -> FetchCount ->  GenRequest 'MtJSON 'RA (WithTotalCount Job)
jobsForWorkflowRunAttemptR owner repo run attempt =
    PagedQuery ["repos", toPathPart owner, toPathPart repo, "actions", "runs", toPathPart run, "attempts", toPathPart attempt, "jobs"] []

-- | List jobs for a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-jobs#list-jobs-for-a-workflow-run>
jobsForWorkflowRunR :: Name Owner -> Name Repo -> Id WorkflowRun -> FetchCount ->  GenRequest 'MtJSON 'RA (WithTotalCount Job)
jobsForWorkflowRunR owner repo run =
    PagedQuery ["repos", toPathPart owner, toPathPart repo, "actions", "runs", toPathPart run, "jobs"] []