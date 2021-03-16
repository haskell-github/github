{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
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
    mkProjectName,
    mkColumnName,
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
    mkProjectId,
    mkColumnId,
    mkCardId,
    -- * IssueNumber
    IssueNumber (..),
    -- * Module re-exports
    module GitHub.Auth,
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
    module GitHub.Data.Repos,
    module GitHub.Data.Projects,
    module GitHub.Data.Request,
    module GitHub.Data.Reviews,
    module GitHub.Data.Search,
    module GitHub.Data.Statuses,
    module GitHub.Data.Teams,
    module GitHub.Data.URL,
    module GitHub.Data.Webhooks,
    module GitHub.Data.Webhooks.Validate,
    ) where

import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Auth
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
import GitHub.Data.Repos
import GitHub.Data.Projects
import GitHub.Data.Request
import GitHub.Data.Reviews
import GitHub.Data.Search
import GitHub.Data.Statuses
import GitHub.Data.Teams
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

mkProjectId :: Int -> Id Project
mkProjectId = Id

mkProjectName :: Text -> Name Project
mkProjectName = N

mkColumnId :: Int -> Id Column
mkColumnId = Id

mkColumnName :: Text -> Name Column
mkColumnName = N

mkCardId :: Int -> Id Card
mkCardId = Id

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
