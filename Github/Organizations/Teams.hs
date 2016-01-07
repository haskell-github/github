{-# LANGUAGE DataKinds #-}
-- | The organization teams API as described on
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
import Github.Auth
import Github.Data
import Github.Request

-- | List teams.  List the teams of an organization.
-- When authenticated, lists private teams visible to the authenticated user.
-- When unauthenticated, lists only public teams for an organization.
--
-- > teamsOf' (Just $ GithubOAuth "token") "thoughtbot"
teamsOf' :: Maybe GithubAuth -> Name Organization -> IO (Either Error [Team])
teamsOf' auth = executeRequestMaybe auth . teamsOfR

-- | List the public teams of an organization.
--
-- > teamsOf "thoughtbot"
teamsOf :: Name Organization -> IO (Either Error [Team])
teamsOf = teamsOf' Nothing

-- | List teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-teams>
teamsOfR :: Name Organization -> GithubRequest k [Team]
teamsOfR organization = GithubGet ["orgs", untagName organization, "teams"] []

-- | The information for a single team, by team id.
-- | With authentication
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor' :: Maybe GithubAuth -> Id Team -> IO (Either Error DetailedTeam)
teamInfoFor' auth tid =
    executeRequestMaybe auth $ teamInfoForR tid

-- | The information for a single team, by team id.
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor :: Id Team -> IO (Either Error DetailedTeam)
teamInfoFor = teamInfoFor' Nothing

-- | Get team.
-- See <https://developer.github.com/v3/orgs/teams/#get-team>
teamInfoForR  :: Id Team -> GithubRequest k DetailedTeam
teamInfoForR tid =
    GithubGet ["teams", show $ untagId tid] []

-- | Create a team under an organization
--
-- > createTeamFor' (GithubOAuth "token") "organization" (CreateTeam "newteamname" "some description" [] PermssionPull)
createTeamFor' :: GithubAuth
               -> Name Organization
               -> CreateTeam
               -> IO (Either Error DetailedTeam)
createTeamFor' auth org cteam =
    executeRequest auth $ createTeamForR org cteam

-- | Create team.
-- See <https://developer.github.com/v3/orgs/teams/#create-team>
createTeamForR :: Name Organization -> CreateTeam -> GithubRequest 'True DetailedTeam
createTeamForR org cteam =
    GithubPost Post ["orgs", untagName org, "teams"] (encode cteam)

-- | Edit a team, by id.
--
-- > editTeamFor'
editTeam' :: GithubAuth
          -> Id DetailedTeam
          -> EditTeam
          -> IO (Either Error DetailedTeam)
editTeam' auth tid eteam =
    executeRequest auth $ editTeamR tid eteam

-- | Edit team.
-- See <https://developer.github.com/v3/orgs/teams/#edit-team>
editTeamR :: Id DetailedTeam -> EditTeam -> GithubRequest 'True DetailedTeam
editTeamR tid eteam =
    GithubPost Patch ["teams", show $ untagId tid] (encode eteam)

-- | Delete a team, by id.
--
-- > deleteTeam' (GithubOAuth "token") 1010101
deleteTeam' :: GithubAuth -> Id DetailedTeam -> IO (Either Error ())
deleteTeam' auth tid =
    executeRequest auth $ deleteTeamR tid

-- | Delete team.
-- See <https://developer.github.com/v3/orgs/teams/#delete-team>
deleteTeamR :: Id DetailedTeam -> GithubRequest 'True ()
deleteTeamR tid =
    GithubDelete ["teams", show $ untagId tid]

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
    GithubGet ["teams", show $ untagId tid, "memberships", untagName user] []

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
    GithubPost Put ["teams", show $ untagId tid, "memberships", untagName user] (encode $ CreateTeamMembership role)

-- | Delete a member of a team.
--
-- > deleteTeamMembershipFor' (GithubOAuth "token") 1010101 "mburns"
deleteTeamMembershipFor' :: GithubAuth -> Id Team -> Name GithubOwner -> IO (Either Error ())
deleteTeamMembershipFor' auth tid user =
    executeRequest auth $ deleteTeamMembershipForR tid user

-- | Remove team membership
-- See <https://developer.github.com/v3/orgs/teams/#remove-team-membership>
deleteTeamMembershipForR :: Id Team -> Name GithubOwner -> GithubRequest 'True ()
deleteTeamMembershipForR tid user =
    GithubDelete ["teams", show $ untagId tid, "memberships", untagName user]

-- | List teams for current authenticated user
--
-- > listTeamsCurrent' (GithubOAuth "token")
listTeamsCurrent' :: GithubAuth -> IO (Either Error [DetailedTeam])
listTeamsCurrent' auth = executeRequest auth $ listTeamsCurrentR

-- | List user teams.
-- See <https://developer.github.com/v3/orgs/teams/#list-user-teams>
listTeamsCurrentR :: GithubRequest 'True [DetailedTeam]
listTeamsCurrentR = GithubGet ["user", "teams"] []
