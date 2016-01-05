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
    -- Missing endpoints:
    --
    -- * List team members
    -- * Get team member (deprecated)
    -- * Add team member (deprecated)
    -- * Remove team member (deprecated)
    -- * List team repos
    -- * Check if a team manages a repository
    -- * Add team repository
    -- * Remove team repository
    teamsOfR,
    teamInfoForR,
    createTeamForR,
    editTeamR,
    deleteTeamR,
    teamMembershipInfoForR,
    addTeamMembershipForR,
    deleteTeamMembershipForR,
    listTeamsCurrentR,

    -- * Search
    -- | See <https://developer.github.com/v3/search/>
   --
    -- Missing endpoints:
    --
    -- * Search users
    searchReposR,
    searchCodeR,
    searchIssuesR,

    -- * Users
    -- | See <https://developer.github.com/v3/users/>
    --
    -- Missing endpoints:
    --
    -- * Update the authenticated user
    -- * Get all users
    userInfoForR,
    userInfoCurrentR,
    -- ** Followers
    -- | See <https://developer.github.com/v3/users/followers/>
    --
    -- Missing endpoints:
    --
    -- * Check if you are following a user
    -- * Check if one user follows another
    -- * Follow a user
    -- * Unfollow a user
    usersFollowingR,
    usersFollowedByR,

    -- * Data definitions
    module Github.Data
    ) where

import Github.Data
import Github.Organizations
import Github.Organizations.Members
import Github.Organizations.Teams
import Github.Search
import Github.Users
import Github.Users.Followers
