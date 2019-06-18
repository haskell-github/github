-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module re-exports all request constructors and data definitions from
-- this package.
--
-- See "GitHub.Request" module for executing 'Request', in short
-- use @'github' request@, for example
--
-- @
-- 'github' 'userInfoForR'
--   :: 'AuthMethod' am => am -> 'Name' 'User' -> IO (Either 'Error' 'User')
-- @
--
-- The missing endpoints lists show which endpoints we know are missing, there
-- might be more.
--
module GitHub (
    -- * Activity
    -- | See <https://developer.github.com/v3/activity/>

    -- ** Events
    -- | See <https://developer.github.com/v3/activity/events/>
    repositoryEventsR,
    userEventsR,

    -- ** Notifications
    -- | See <https://developer.github.com/v3/activity/notifications/>
    getNotificationsR,
    markNotificationAsReadR,
    markAllNotificationsAsReadR,

    -- ** Starring
    -- | See <https://developer.github.com/v3/activity/starring/>
    --
    -- Missing endpoints:
    --
    -- * Check if you are starring a repository
    stargazersForR,
    reposStarredByR,
    myStarredR,
    myStarredAcceptStarR,
    starRepoR,
    unstarRepoR,

    -- ** Watching
    -- | See <https://developer.github.com/v3/activity/>
    --
    -- Missing endpoints:
    --
    -- * Query a Repository Subscription
    -- * Set a Repository Subscription
    -- * Delete a Repository Subscription
    watchersForR,
    reposWatchedByR,

    -- * Gists
    -- | See <https://developer.github.com/v3/gists/>
    --
    -- Missing endpoints:
    --
    -- * Query a specific revision of a gist
    -- * Create a gist
    -- * Edit a gist
    -- * List gist commits
    -- * Check if a gist is starred
    -- * Fork a gist
    -- * List gist forks
    gistsR,
    gistR,
    starGistR,
    unstarGistR,
    deleteGistR,

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
    gitCommitR,

    -- ** References
    -- | See <https://developer.github.com/v3/git/refs/>
    referenceR,
    referencesR,
    createReferenceR,
    deleteReferenceR,
    namespacedReferencesR,

    -- ** Trees
    -- | See <https://developer.github.com/v3/git/trees/>
    treeR,
    nestedTreeR,

    -- * Issues
    -- | See <https://developer.github.com/v3/issues/>
    --
    currentUserIssuesR,
    organizationIssuesR,
    issueR,
    issuesForRepoR,
    createIssueR,
    editIssueR,

    -- ** Comments
    -- | See <https://developer.github.com/v3/issues/comments/>
    --
    commentR,
    commentsR,
    createCommentR,
    deleteCommentR,
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
    milestonesR,
    milestoneR,
    createMilestoneR,
    updateMilestoneR,
    deleteMilestoneR,

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
    organizationsR,
    -- ** Members
    -- | See <https://developer.github.com/v3/orgs/members/>
    --
    -- Missing endpoints: All except /Members List/ and /Check Membership/
    membersOfR,
    membersOfWithR,
    isMemberOfR,
    orgInvitationsR,
    -- ** Outside Collaborators
    -- | See <https://developer.github.com/v3/orgs/outside_collaborators/>
    --
    -- Missing endpoints: All except /Outside Collaborator List/
    outsideCollaboratorsR,

    -- ** Teams
    -- | See <https://developer.github.com/v3/orgs/teams/>
    --
    -- Missing endpoints:
    --
    -- * Query team member (deprecated)
    -- * Add team member (deprecated)
    -- * Remove team member (deprecated)
    -- * Check if a team manages a repository
    -- * Add team repository
    -- * Remove team repository
    teamsOfR,
    teamInfoForR,
    createTeamForR,
    editTeamR,
    deleteTeamR,
    listTeamMembersR,
    listTeamReposR,
    teamMembershipInfoForR,
    addTeamMembershipForR,
    deleteTeamMembershipForR,
    listTeamsCurrentR,

    -- * Pull Requests
    -- | See <https://developer.github.com/v3/pulls/>
    pullRequestsForR,
    pullRequestR,
    pullRequestPatchR,
    pullRequestDiffR,
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
    -- * Edit a comment
    -- * Delete a comment
    pullRequestCommentsR,
    pullRequestCommentR,
    createPullCommentR,
    createPullCommentReplyR,

    -- ** Pull request reviews
    -- | See <https://developer.github.com/v3/pulls/reviews/>
    --
    -- Missing endpoints:
    --
    -- * Delete a pending review
    -- * Create a pull request review
    -- * Submit a pull request review
    -- * Dismiss a pull request review
    pullRequestReviewsR,
    pullRequestReviewR,
    pullRequestReviewCommentsR,

    -- * Repositories
    -- | See <https://developer.github.com/v3/repos/>
    --
    -- Missing endpoints:
    --
    -- * List all public repositories
    -- * List Teams
    -- * Query Branch
    -- * Enabling and disabling branch protection
    currentUserReposR,
    userReposR,
    organizationReposR,
    repositoryR,
    contributorsR,
    languagesForR,
    tagsForR,
    branchesForR,

    -- ** Collaborators
    -- | See <https://developer.github.com/v3/repos/collaborators/>
    collaboratorsOnR,
    collaboratorPermissionOnR,
    isCollaboratorOnR,
    addCollaboratorR,

    -- ** Comments
    -- | See <https://developer.github.com/v3/repos/comments/>
    --
    -- Missing endpoints:
    --
    -- * Create a commit comment
    -- * Update a commit comment
    -- * Delete a commit comment
    commentsForR,
    commitCommentsForR,
    commitCommentForR,

    -- ** Commits
    -- | See <https://developer.github.com/v3/repos/commits/>
    commitsForR,
    commitsWithOptionsForR,
    commitR,
    diffR,

    -- ** Contents
    -- | See <https://developer.github.com/v3/repos/contents/>
    contentsForR,
    readmeForR,
    archiveForR,
    createFileR,
    updateFileR,
    deleteFileR,

    -- ** Deploy Keys
    -- | See <https://developer.github.com/v3/repos/keys/>
    deployKeysForR,
    deployKeyForR,
    createRepoDeployKeyR,
    deleteRepoDeployKeyR,

    -- ** Deployments
    -- | See <https://developer.github.com/v3/repos/deployments/#deployments>
    --
    -- Missing endpoints:
    -- * Get a single deployment
    -- * Update a deployment
    -- * Get a single deployment status
    deploymentsWithOptionsForR,
    createDeploymentR,
    deploymentStatusesForR,
    createDeploymentStatusR,

    -- ** Forks
    -- | See <https://developer.github.com/v3/repos/forks/>
    --
    -- Missing endpoints:
    --
    -- * Create a fork
    forksForR,

    -- ** Statuses
    -- | See <https://developer.github.com/v3/repos/statuses/>
    createStatusR,
    statusesForR,
    statusForR,

    -- ** Webhooks
    -- | See <https://developer.github.com/v3/repos/hooks/>
    webhooksForR,
    webhookForR,
    createRepoWebhookR,
    editRepoWebhookR,
    testPushRepoWebhookR,
    pingRepoWebhookR,
    deleteRepoWebhookR,

    -- * Releases
    releasesR,
    releaseR,
    latestReleaseR,
    releaseByTagNameR,

    -- ** Invitations
    -- | See <https://developer.github.com/v3/repos/invitations/>
    -- Missing endpoints:

    -- * Delete a repository invitation
    -- * Update a repository invitation
    -- * Decline a repository invitation

    listInvitationsOnR,
    acceptInvitationFromR,
    listInvitationsForR,


    -- * Search
    -- | See <https://developer.github.com/v3/search/>
    searchReposR,
    searchCodeR,
    searchIssuesR,
    searchUsersR,

    -- * Users
    -- | See <https://developer.github.com/v3/users/>
    --
    -- Missing endpoints:
    --
    -- * Update the authenticated user
    -- * Query all users
    userInfoForR,
    ownerInfoForR,
    userInfoCurrentR,

    -- ** Emails
    -- | See <https://developer.github.com/v3/users/emails/>
    --
    -- Missing endpoints:
    --
    -- * Add email address(es)
    -- * Delete email address(es)
    -- * Toggle primary email visibility
    currentUserEmailsR,
    currentUserPublicEmailsR,

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

    -- ** Git SSH Keys
    -- | See <https://developer.github.com/v3/users/keys/>
    publicSSHKeysR,
    publicSSHKeysForR,
    publicSSHKeyR,
    createUserPublicSSHKeyR,
    deleteUserPublicSSHKeyR,

    -- ** Rate Limit
    -- | See <https://developer.github.com/v3/rate_limit/>
    rateLimitR,

    -- * Data definitions
    module GitHub.Data,
    -- * Request handling
    module GitHub.Request,
    ) where

import GitHub.Data
import GitHub.Endpoints.Activity.Events
import GitHub.Endpoints.Activity.Notifications
import GitHub.Endpoints.Activity.Starring
import GitHub.Endpoints.Activity.Watching
import GitHub.Endpoints.Gists
import GitHub.Endpoints.Gists.Comments
import GitHub.Endpoints.GitData.Blobs
import GitHub.Endpoints.GitData.Commits
import GitHub.Endpoints.GitData.References
import GitHub.Endpoints.GitData.Trees
import GitHub.Endpoints.Issues
import GitHub.Endpoints.Issues.Comments
import GitHub.Endpoints.Issues.Events
import GitHub.Endpoints.Issues.Labels
import GitHub.Endpoints.Issues.Milestones
import GitHub.Endpoints.Organizations
import GitHub.Endpoints.Organizations.Members
import GitHub.Endpoints.Organizations.OutsideCollaborators
import GitHub.Endpoints.Organizations.Teams
import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.PullRequests.Comments
import GitHub.Endpoints.PullRequests.Reviews
import GitHub.Endpoints.RateLimit
import GitHub.Endpoints.Repos
import GitHub.Endpoints.Repos.Collaborators
import GitHub.Endpoints.Repos.Comments
import GitHub.Endpoints.Repos.Commits
import GitHub.Endpoints.Repos.Contents
import GitHub.Endpoints.Repos.DeployKeys
import GitHub.Endpoints.Repos.Deployments
import GitHub.Endpoints.Repos.Forks
import GitHub.Endpoints.Repos.Invitations
import GitHub.Endpoints.Repos.Releases
import GitHub.Endpoints.Repos.Statuses
import GitHub.Endpoints.Repos.Webhooks
import GitHub.Endpoints.Search
import GitHub.Endpoints.Users
import GitHub.Endpoints.Users.Emails
import GitHub.Endpoints.Users.Followers
import GitHub.Endpoints.Users.PublicSSHKeys
import GitHub.Request
