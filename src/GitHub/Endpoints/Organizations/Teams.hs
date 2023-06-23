-- |
-- The Owner teams API as described on
-- <http://developer.github.com/v3/orgs/teams/>.

module GitHub.Endpoints.Organizations.Teams (
    teamsOfR,
    teamInfoForR,
    createTeamForR,
    editTeamR,
    deleteTeamR,
    listTeamMembersR,
    listTeamReposR,
    addOrUpdateTeamRepoR,
    teamMembershipInfoForR,
    addTeamMembershipForR,
    deleteTeamMembershipForR,
    listTeamsCurrentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-teams>
teamsOfR :: Name Organization -> FetchCount -> Request k (Vector SimpleTeam)
teamsOfR org =
    pagedQuery ["orgs", toPathPart org, "teams"] []

-- | Query team.
-- See <https://developer.github.com/v3/orgs/teams/#get-team>
teamInfoForR  :: Id Team -> Request k Team
teamInfoForR tid =
    query ["teams", toPathPart tid] []

-- | Create team.
-- See <https://developer.github.com/v3/orgs/teams/#create-team>
createTeamForR :: Name Organization -> CreateTeam -> Request 'RW Team
createTeamForR org cteam =
    command Post ["orgs", toPathPart org, "teams"] (encode cteam)

-- | Edit team.
-- See <https://developer.github.com/v3/orgs/teams/#edit-team>
editTeamR :: Id Team -> EditTeam -> Request 'RW Team
editTeamR tid eteam =
    command Patch ["teams", toPathPart tid] (encode eteam)

--
-- See <https://developer.github.com/v3/orgs/teams/#delete-team>
deleteTeamR :: Id Team -> GenRequest 'MtUnit 'RW ()
deleteTeamR tid =
    Command Delete ["teams", toPathPart tid] mempty

-- | List team members.
--
-- See <https://developer.github.com/v3/orgs/teams/#list-team-members>
listTeamMembersR :: Id Team -> TeamMemberRole -> FetchCount -> Request 'RA (Vector SimpleUser)
listTeamMembersR tid r =
    pagedQuery ["teams", toPathPart tid, "members"] [("role", Just r')]
  where
    r' = case r of
        TeamMemberRoleAll         -> "all"
        TeamMemberRoleMaintainer  -> "maintainer"
        TeamMemberRoleMember      -> "member"

-- | Query team repositories.
-- See <https://developer.github.com/v3/orgs/teams/#list-team-repos>
listTeamReposR :: Id Team -> FetchCount -> Request k (Vector Repo)
listTeamReposR tid  =
    pagedQuery ["teams", toPathPart tid, "repos"] []

-- | Add or update a team repository.
-- See <https://developer.github.com/v3/orgs/teams/#add-or-update-team-repository>
addOrUpdateTeamRepoR :: Id Team -> Name Organization -> Name Repo -> Permission -> GenRequest 'MtUnit 'RW ()
addOrUpdateTeamRepoR tid org repo permission =
    Command Put ["teams", toPathPart tid, "repos", toPathPart org, toPathPart repo] (encode $ AddTeamRepoPermission permission)

-- | Query team membership.
-- See <https://developer.github.com/v3/orgs/teams/#get-team-membership
teamMembershipInfoForR :: Id Team -> Name Owner -> Request k TeamMembership
teamMembershipInfoForR tid user =
    query ["teams", toPathPart tid, "memberships", toPathPart user] []

-- | Add team membership.
-- See <https://developer.github.com/v3/orgs/teams/#add-team-membership>
addTeamMembershipForR :: Id Team -> Name Owner -> Role -> Request 'RW TeamMembership
addTeamMembershipForR tid user role =
    command Put ["teams", toPathPart tid, "memberships", toPathPart user] (encode $ CreateTeamMembership role)

-- | Remove team membership.
-- See <https://developer.github.com/v3/orgs/teams/#remove-team-membership>
deleteTeamMembershipForR :: Id Team -> Name Owner -> GenRequest 'MtUnit 'RW ()
deleteTeamMembershipForR tid user =
    Command Delete ["teams", toPathPart tid, "memberships", toPathPart user] mempty

-- | List user teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-user-teams>
listTeamsCurrentR :: FetchCount -> Request 'RA (Vector Team)
listTeamsCurrentR =
    pagedQuery ["user", "teams"] []
