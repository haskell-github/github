-- |
--
-- This module re-exports all request constructrors and
-- data definitions from this package.
module Github.All (
    -- * Organizations
    -- | See <https://developer.github.com/v3/orgs/>
    --
    -- Missing endpoints:
    --
    -- * List your organizations
    -- * List all organizations
    -- * Edit an organization
    publicOrganizationsForR,
    publicOrganizationR,
    -- ** Members
    -- | See <https://developer.github.com/v3/orgs/members/>
    --
    -- Missing endpoints: All except /Members List/
    membersOfR,
    -- ** Teams
    -- | See <https://developer.github.com/v3/orgs/teams/>
    --
    -- Missing endpoints: All except /List teams/
    teamsOfR,
    -- * Data definitions
    module Github.Data
    ) where

import Github.Data
import Github.Organizations
import Github.Organizations.Members
import Github.Organizations.Teams
