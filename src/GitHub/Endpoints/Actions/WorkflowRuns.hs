-- -----------------------------------------------------------------------------
-- -- |
-- -- License     :  BSD-3-Clause
-- -- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- --
-- -- The pull requests API as documented at
-- -- <http://developer.github.com/v3/pulls/>.
module GitHub.Endpoints.Actions.WorkflowRuns (
    reRunJobR,
    workflowRunsR,
    workflowRunR,
    deleteWorkflowRunR,
    workflowRunReviewHistoryR,
    approveWorkflowRunR,
    workflowRunAttemptR,
    downloadWorkflowRunAttemptLogsR,
    cancelWorkflowRunR,
    downloadWorkflowRunLogsR,
    deleteWorkflowRunLogsR,
    reRunWorkflowR,
    reRunFailedJobsR,
    workflowRunsForWorkflowR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Network.URI             (URI)
import Prelude ()

-- | Re-run a job from a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#re-run-a-job-from-a-workflow-run>
reRunJobR
    :: Name Owner
    -> Name Repo
    -> Id Job ->  GenRequest 'MtJSON 'RW ()
reRunJobR user repo job = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "jobs", toPathPart job, "rerun"]
    mempty

-- | List workflow runs for a repository.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#list-workflow-runs-for-a-repository>
workflowRunsR
    :: Name Owner
    -> Name Repo
    -> FetchCount ->  GenRequest 'MtJSON 'RA (WithTotalCount WorkflowRun)
workflowRunsR user repo = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "runs"]
    []

-- | Get a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#get-a-workflow-run>
workflowRunR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RA (WithTotalCount WorkflowRun)
workflowRunR user repo run = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run]
    []

-- | Delete a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#delete-a-workflow-run>
deleteWorkflowRunR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RW ()
deleteWorkflowRunR user repo run = Command Delete
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run]
    mempty

-- | Get the review history for a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#get-the-review-history-for-a-workflow-run>
workflowRunReviewHistoryR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RA (Vector ReviewHistory)
workflowRunReviewHistoryR user repo run = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "approvals"]
    []

-- | Approve a workflow run for a fork pull request.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#approve-a-workflow-run-for-a-fork-pull-request>
approveWorkflowRunR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RW ()
approveWorkflowRunR user repo run = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "approve"]
    mempty

-- | Get a workflow run attempt.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#get-a-workflow-run-attempt>
workflowRunAttemptR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    -> Id RunAttempt
    ->  GenRequest 'MtJSON 'RA (WithTotalCount WorkflowRun)
workflowRunAttemptR user repo run attempt = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "attempts", toPathPart attempt]
    []

-- | Download workflow run attempt logs.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#download-workflow-run-attempt-logs>
downloadWorkflowRunAttemptLogsR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    -> Id RunAttempt
    -> GenRequest 'MtRedirect 'RO URI
downloadWorkflowRunAttemptLogsR user repo run attempt = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "attempts", toPathPart attempt, "logs"]
    []

-- | Cancel a workflow run.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#cancel-a-workflow-run>
cancelWorkflowRunR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RW ()
cancelWorkflowRunR user repo run = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "cancel"]
    mempty

-- | Download workflow run logs.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#download-workflow-run-logs>
downloadWorkflowRunLogsR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    -> GenRequest 'MtRedirect 'RO URI
downloadWorkflowRunLogsR user repo run = Query
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "logs"]
    []

-- | Delete workflow run logs.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#delete-workflow-run-logs>
deleteWorkflowRunLogsR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun
    ->  GenRequest 'MtJSON 'RW ()
deleteWorkflowRunLogsR user repo run = Command Delete
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "logs"]
    mempty

-- | Re-run a workflow.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#re-run-a-workflow>
reRunWorkflowR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun->  GenRequest 'MtJSON 'RW ()
reRunWorkflowR user repo run = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "rerun"]
    mempty

-- | Re-run failed jobs from a workflow run.
-- See <https://docs.github.com/en/rest/actions/re-run-failed-jobs-from-a-workflow-run>
reRunFailedJobsR
    :: Name Owner
    -> Name Repo
    -> Id WorkflowRun->  GenRequest 'MtJSON 'RW ()
reRunFailedJobsR user repo run = Command Post
    ["repos", toPathPart user, toPathPart repo, "actions", "runs", toPathPart run, "rerun-failed-jobs"]
    mempty

-- | List workflow runs for a workflow.
-- See <https://docs.github.com/en/rest/actions/workflow-runs#list-workflow-runs-for-a-workflow>
workflowRunsForWorkflowR
    :: Name Owner
    -> Name Repo
    -> Id Workflow
    -> FetchCount -> GenRequest 'MtJSON 'RA (WithTotalCount WorkflowRun)
workflowRunsForWorkflowR user repo workflow = PagedQuery
    ["repos", toPathPart user, toPathPart repo, "actions", "workflows", toPathPart workflow, "runs"]
    []
