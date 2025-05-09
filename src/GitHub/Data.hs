-- |
-- This module re-exports the @GitHub.Data.@ and "GitHub.Auth" submodules.

module GitHub.Data (
    -- * Tagged types
    -- ** Name
    Name,
    mkName,
    untagName,
    mkOwnerName,
    mkUserName,
    mkTeamName,
    mkOrganizationName,
    mkRepoName,
    mkCommitName,
    fromUserName,
    fromOrganizationName,
    -- ** Id
    Id,
    mkId,
    untagId,
    mkOwnerId,
    mkUserId,
    mkTeamId,
    mkOrganizationId,
    mkRepoId,
    fromUserId,
    fromOrganizationId,
    -- * IssueNumber
    IssueNumber (..),
    -- * Module re-exports
    module GitHub.Auth,
    module GitHub.Data.Actions.Common,
    module GitHub.Data.Actions.Artifacts,
    module GitHub.Data.Actions.Cache,
    module GitHub.Data.Actions.Secrets,
    module GitHub.Data.Actions.Workflows,
    module GitHub.Data.Actions.WorkflowJobs,
    module GitHub.Data.Actions.WorkflowRuns,
    module GitHub.Data.Activities,
    module GitHub.Data.Comments,
    module GitHub.Data.Content,
    module GitHub.Data.Definitions,
    module GitHub.Data.DeployKeys,
    module GitHub.Data.Deployments,
    module GitHub.Data.Email,
    module GitHub.Data.Events,
    module GitHub.Data.Gists,
    module GitHub.Data.GitData,
    module GitHub.Data.Invitation,
    module GitHub.Data.Issues,
    module GitHub.Data.Milestone,
    module GitHub.Data.Options,
    module GitHub.Data.PublicSSHKeys,
    module GitHub.Data.PullRequests,
    module GitHub.Data.RateLimit,
    module GitHub.Data.Releases,
    module GitHub.Data.Reactions,
    module GitHub.Data.Repos,
    module GitHub.Data.Request,
    module GitHub.Data.Reviews,
    module GitHub.Data.Search,
    module GitHub.Data.Statuses,
    module GitHub.Data.Teams,
    module GitHub.Data.Traffic,
    module GitHub.Data.URL,
    module GitHub.Data.Webhooks,
    module GitHub.Data.Webhooks.Validate,
    ) where

import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Auth
import GitHub.Data.Actions.Common
import GitHub.Data.Actions.Artifacts
import GitHub.Data.Actions.Secrets
import GitHub.Data.Actions.Cache
import GitHub.Data.Actions.Workflows
import GitHub.Data.Actions.WorkflowJobs
import GitHub.Data.Actions.WorkflowRuns
import GitHub.Data.Activities
import GitHub.Data.Comments
import GitHub.Data.Content
import GitHub.Data.Definitions
import GitHub.Data.DeployKeys
import GitHub.Data.Deployments
import GitHub.Data.Email
import GitHub.Data.Events
import GitHub.Data.Gists
import GitHub.Data.GitData
import GitHub.Data.Id
import GitHub.Data.Invitation
import GitHub.Data.Issues
import GitHub.Data.Milestone
import GitHub.Data.Name
import GitHub.Data.Options
import GitHub.Data.PublicSSHKeys
import GitHub.Data.PullRequests
import GitHub.Data.RateLimit
import GitHub.Data.Releases
import GitHub.Data.Reactions
import GitHub.Data.Repos
import GitHub.Data.Request
import GitHub.Data.Reviews
import GitHub.Data.Search
import GitHub.Data.Statuses
import GitHub.Data.Teams
import GitHub.Data.Traffic
import GitHub.Data.URL
import GitHub.Data.Webhooks
import GitHub.Data.Webhooks.Validate

mkOwnerId :: Int -> Id Owner
mkOwnerId = Id

mkOwnerName :: Text -> Name Owner
mkOwnerName = N

mkUserId :: Int -> Id User
mkUserId = Id

mkUserName :: Text -> Name User
mkUserName = N

mkTeamId :: Int -> Id Team
mkTeamId = Id

mkTeamName :: Text -> Name Team
mkTeamName = N

mkOrganizationId :: Int -> Id Organization
mkOrganizationId = Id

mkOrganizationName :: Text -> Name Organization
mkOrganizationName = N

mkRepoId :: Int -> Id Repo
mkRepoId = Id

mkRepoName :: Text -> Name Repo
mkRepoName = N

mkCommitName :: Text -> Name Commit
mkCommitName = N

fromOrganizationName :: Name Organization -> Name Owner
fromOrganizationName = N . untagName

fromUserName :: Name User -> Name Owner
fromUserName = N . untagName

fromOrganizationId :: Id Organization -> Id Owner
fromOrganizationId = Id . untagId

fromUserId :: Id User -> Id Owner
fromUserId = Id . untagId
