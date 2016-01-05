-- |
--
-- This module re-exports all request constructrors and
-- data definitions from this package.
module Github.All (
     -- * Gists
    -- | See <https://developer.github.com/v3/gists/>
    --
    -- Missing endpoints:
    --
    -- * Get a specific revision of a gist
    -- * Create a gist
    -- * Edit a gist
    -- * List gist commits
    -- * Star a gist
    -- * Unstar a gist
    -- * Check if a gist is starred
    -- * Fork a gist
    -- * List gist forks
    -- * Delete a gist
    gistsR,
    gistR,

    -- ** Comments
    -- | See <https://developer.github.com/v3/gists/comments/>
    --
    -- Missing endpoints:
    -- * Create a comment
    -- * Edit a comment
    -- * Delete a comment
    commentsOnR,
    gistCommentR,
   
    -- * Issues
    -- | See <https://developer.github.com/v3/issues/>
    --
    -- Missing endpoints: 
    --
    -- * List issues
    issueR,
    issuesForRepoR,
    createIssueR,
    editIssueR,

    -- ** Comments
    -- | See <https://developer.github.com/v3/issues/comments/>
    --
    -- Missing endpoints:
    --
    -- * Delete comment
    commentR,
    commentsR,
    createCommentR,
    editCommentR,

    -- ** Events
    -- | See <https://developer.github.com/v3/issues/events/>
    --
    eventsForIssueR,
    eventsForRepoR,
    eventR,

    -- ** Labels
    -- | See <https://developer.github.com/v3/issues/labels/>
    --
    labelsOnRepoR,
    labelR,
    createLabelR,
    updateLabelR,
    deleteLabelR,
    labelsOnIssueR,
    addLabelsToIssueR,
    removeLabelFromIssueR,
    replaceAllLabelsForIssueR,
    removeAllLabelsFromIssueR,
    labelsOnMilestoneR,

    -- ** Milestone
    -- | See <https://developer.github.com/v3/issues/milestones/>
    -- 
    -- Missing endpoints:
    --
    -- * Create a milestone
    -- * Update a milestone
    -- * Delete a milestone
    milestonesR,
    milestoneR,

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
import Github.Issues
import Github.Issues.Comments
import Github.Issues.Events
import Github.Issues.Labels
import Github.Issues.Milestones
import Github.Gists
import Github.Gists.Comments
import Github.Organizations
import Github.Organizations.Members
import Github.Organizations.Teams
import Github.Search
import Github.Users
import Github.Users.Followers
