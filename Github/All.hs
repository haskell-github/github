-- |
--
-- This module re-exports all request constructrors and
-- data definitions from this package.
module Github.All (
    -- * Activity
    -- | See <https://developer.github.com/v3/activity/>

    -- ** Starring
    -- | See <https://developer.github.com/v3/activity/starring/>
    --
    -- Missing endpoints:
    --
    -- * Check if you are starring a repository
    -- * Star a repository
    -- * Unstar a repository
    stargazersForR,
    reposStarredByR,
    myStarredR,

    -- ** Watching
    -- | See <https://developer.github.com/v3/activity/>
    --
    -- Missing endpoints:
    --
    -- * Get a Repository Subscription
    -- * Set a Repository Subscription
    -- * Delete a Repository Subscription
    watchersForR,
    reposWatchedByR,

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

    -- * Git Data
    -- | See <https://developer.github.com/v3/git/>

    -- ** Blobs
    -- | See <https://developer.github.com/v3/git/blobs/>
    blobR,

    -- ** Commits
    -- | See <https://developer.github.com/v3/git/commits/>
    commitR,

    -- ** References
    -- | See <https://developer.github.com/v3/git/refs/>
    referenceR,
    referencesR,
    createReferenceR,

    -- ** Trees
    -- | See <https://developer.github.com/v3/git/trees/>
    treeR,
    nestedTreeR,

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

    -- * Pull Requests
    -- | See <https://developer.github.com/v3/pulls/>
    pullRequestsForR,
    pullRequestR,
    createPullRequestR,
    updatePullRequestR,
    pullRequestCommitsR,
    pullRequestFilesR,
    isPullRequestMergedR,
    mergePullRequestR,

    -- ** Review comments
    -- | See <https://developer.github.com/v3/pulls/comments/>
    --
    -- Missing endpoints:
    --
    -- * List comments in a repository
    -- * Create a comment
    -- * Edit a comment
    -- * Delete a comment
    pullRequestReviewCommentsR,
    pullRequestReviewCommentR,

    -- * Repositories
    -- | See <https://developer.github.com/v3/repos/>

    -- ** Collaborators

    -- ** Commits

    -- ** Forks

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

import Github.Activity.Starring
import Github.Activity.Watching
import Github.Data
import Github.Gists
import Github.Gists.Comments
import Github.GitData.Blobs
import Github.GitData.Commits
import Github.GitData.References
import Github.GitData.Trees
import Github.Issues
import Github.Issues.Comments
import Github.Issues.Events
import Github.Issues.Labels
import Github.Issues.Milestones
import Github.Organizations
import Github.Organizations.Members
import Github.Organizations.Teams
import Github.PullRequests
import Github.PullRequests.ReviewComments
import Github.Search
import Github.Users
import Github.Users.Followers
