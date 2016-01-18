{-# LANGUAGE DataKinds #-}
-- | The GithubOwner teams API as described on
-- <http://developer.github.com/v3/orgs/teams/>.
module Github.Organizations.Teams (
    teamsOf,
    teamsOf',
    teamsOfR,
    teamInfoFor,
    teamInfoFor',
    teamInfoForR,
    createTeamFor',
    createTeamForR,
    editTeam',
    editTeamR,
    deleteTeam',
    deleteTeamR,
    teamMembershipInfoFor,
    teamMembershipInfoFor',
    teamMembershipInfoForR,
    addTeamMembershipFor',
    addTeamMembershipForR,
    deleteTeamMembershipFor',
    deleteTeamMembershipForR,
    listTeamsCurrent',
    listTeamsCurrentR,
    module Github.Data,
    ) where

import Data.Aeson.Compat (encode)
import Data.Vector       (Vector)

import Github.Data
import Github.Request

-- | List teams.  List the teams of an GithubOwner.
-- When authenticated, lists private teams visible to the authenticated user.
-- When unauthenticated, lists only public teams for an GithubOwner.
--
-- > teamsOf' (Just $ GithubOAuth "token") "thoughtbot"
teamsOf' :: Maybe GithubAuth -> Name Organization -> IO (Either Error (Vector SimpleTeam))
teamsOf' auth org =
    executeRequestMaybe auth $ teamsOfR org Nothing

-- | List the public teams of an GithubOwner.
--
-- > teamsOf "thoughtbot"
teamsOf :: Name Organization -> IO (Either Error (Vector SimpleTeam))
teamsOf = teamsOf' Nothing

-- | List teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-teams>
teamsOfR :: Name Organization -> Maybe Count -> GithubRequest k (Vector SimpleTeam)
teamsOfR org = GithubPagedGet ["orgs", toPathPart org, "teams"] []

-- | The information for a single team, by team id.
-- | With authentication
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor' :: Maybe GithubAuth -> Id Team -> IO (Either Error Team)
teamInfoFor' auth tid =
    executeRequestMaybe auth $ teamInfoForR tid

-- | The information for a single team, by team id.
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor :: Id Team -> IO (Either Error Team)
teamInfoFor = teamInfoFor' Nothing

-- | Get team.
-- See <https://developer.github.com/v3/orgs/teams/#get-team>
teamInfoForR  :: Id Team -> GithubRequest k Team
teamInfoForR tid =
    GithubGet ["teams", toPathPart tid] []

-- | Create a team under an GithubOwner
--
-- > createTeamFor' (GithubOAuth "token") "GithubOwner" (CreateTeam "newteamname" "some description" [] PermssionPull)
createTeamFor' :: GithubAuth
               -> Name Organization
               -> CreateTeam
               -> IO (Either Error Team)
createTeamFor' auth org cteam =
    executeRequest auth $ createTeamForR org cteam

-- | Create team.
-- See <https://developer.github.com/v3/orgs/teams/#create-team>
createTeamForR :: Name Organization -> CreateTeam -> GithubRequest 'True Team
createTeamForR org cteam =
    GithubPost Post ["orgs", toPathPart org, "teams"] (encode cteam)

-- | Edit a team, by id.
--
-- > editTeamFor'
editTeam' :: GithubAuth
          -> Id Team
          -> EditTeam
          -> IO (Either Error Team)
editTeam' auth tid eteam =
    executeRequest auth $ editTeamR tid eteam

-- | Edit team.
-- See <https://developer.github.com/v3/orgs/teams/#edit-team>
editTeamR :: Id Team -> EditTeam -> GithubRequest 'True Team
editTeamR tid eteam =
    GithubPost Patch ["teams", toPathPart tid] (encode eteam)

-- | Delete a team, by id.
--
-- > deleteTeam' (GithubOAuth "token") 1010101
deleteTeam' :: GithubAuth -> Id Team -> IO (Either Error ())
deleteTeam' auth tid =
    executeRequest auth $ deleteTeamR tid

-- | Delete team.
-- See <https://developer.github.com/v3/orgs/teams/#delete-team>
deleteTeamR :: Id Team -> GithubRequest 'True ()
deleteTeamR tid =
    GithubDelete ["teams", toPathPart tid]

-- | Retrieve team mebership information for a user.
-- | With authentication
--
-- > teamMembershipInfoFor' (Just $ GithubOAuth "token") 1010101 "mburns"
teamMembershipInfoFor' :: Maybe GithubAuth -> Id Team -> Name GithubOwner -> IO (Either Error TeamMembership)
teamMembershipInfoFor' auth tid user =
    executeRequestMaybe auth $ teamMembershipInfoForR tid user

-- | Get team membership.
-- See <https://developer.github.com/v3/orgs/teams/#get-team-membership
teamMembershipInfoForR :: Id Team -> Name GithubOwner -> GithubRequest k TeamMembership
teamMembershipInfoForR tid user =
    GithubGet ["teams", toPathPart tid, "memberships", toPathPart user] []

-- | Retrieve team mebership information for a user.
--
-- > teamMembershipInfoFor 1010101 "mburns"
teamMembershipInfoFor :: Id Team -> Name GithubOwner -> IO (Either Error TeamMembership)
teamMembershipInfoFor = teamMembershipInfoFor' Nothing

-- | Add (or invite) a member to a team.
--
-- > addTeamMembershipFor' (GithubOAuth "token") 1010101 "mburns" RoleMember
addTeamMembershipFor' :: GithubAuth -> Id Team -> Name GithubOwner -> Role-> IO (Either Error TeamMembership)
addTeamMembershipFor' auth tid user role =
    executeRequest auth $ addTeamMembershipForR tid user role

-- | Add team membership.
-- See <https://developer.github.com/v3/orgs/teams/#add-team-membership>
addTeamMembershipForR :: Id Team -> Name GithubOwner -> Role -> GithubRequest 'True TeamMembership
addTeamMembershipForR tid user role =
    GithubPost Put ["teams", toPathPart tid, "memberships", toPathPart user] (encode $ CreateTeamMembership role)

-- | Delete a member of a team.
--
-- > deleteTeamMembershipFor' (GithubOAuth "token") 1010101 "mburns"
deleteTeamMembershipFor' :: GithubAuth -> Id Team -> Name GithubOwner -> IO (Either Error ())
deleteTeamMembershipFor' auth tid user =
    executeRequest auth $ deleteTeamMembershipForR tid user

-- | Remove team membership.
-- See <https://developer.github.com/v3/orgs/teams/#remove-team-membership>
deleteTeamMembershipForR :: Id Team -> Name GithubOwner -> GithubRequest 'True ()
deleteTeamMembershipForR tid user =
    GithubDelete ["teams", toPathPart tid, "memberships", toPathPart user]

-- | List teams for current authenticated user
--
-- > listTeamsCurrent' (GithubOAuth "token")
listTeamsCurrent' :: GithubAuth -> IO (Either Error (Vector Team))
listTeamsCurrent' auth = executeRequest auth $ listTeamsCurrentR Nothing

-- | List user teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-user-teams>
listTeamsCurrentR :: Maybe Count -> GithubRequest 'True (Vector Team)
listTeamsCurrentR = GithubPagedGet ["user", "teams"] []
