module Github.Teams (
 teamInfoFor
,teamInfoFor'
,teamsInfo'
,createTeamFor'
,editTeam'
,deleteTeam'
,listTeamsCurrent'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The information for a single team, by team id.
-- | With authentication
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor' :: Maybe GithubAuth -> Int -> IO (Either Error DetailedTeam)
teamInfoFor' auth team_id = githubGet' auth ["teams", show team_id]

-- | The information for a single team, by team id.
--
-- > teamInfoFor' (Just $ GithubOAuth "token") 1010101
teamInfoFor :: Int -> IO (Either Error DetailedTeam)
teamInfoFor = teamInfoFor' Nothing

-- | Lists all teams, across all organizations, that the current user belongs to.
--
-- > teamsInfo' (Just $ GithubOAuth "token")
teamsInfo' :: Maybe GithubAuth -> IO (Either Error [DetailedTeam])
teamsInfo' auth = githubGet' auth ["user", "teams"]

-- | Create a team under an organization
--
-- > createTeamFor' (GithubOAuth "token") "organization" (CreateTeam "newteamname" "some description" [] PermssionPull)
createTeamFor' :: GithubAuth
               -> String
               -> CreateTeam
               -> IO (Either Error DetailedTeam)
createTeamFor' auth organization create_team =
  githubPost auth ["orgs", organization, "teams"] create_team

-- | Edit a team, by id.
--
-- > editTeamFor'
editTeam' :: GithubAuth
          -> Int
          -> EditTeam
          -> IO (Either Error DetailedTeam)
editTeam' auth team_id edit_team =
  githubPatch auth ["teams", show team_id] edit_team

-- | Delete a team, by id.
--
-- > deleteTeam' (GithubOAuth "token") 1010101
deleteTeam' :: GithubAuth -> Int -> IO (Either Error ())
deleteTeam' auth team_id = githubDelete auth ["teams", show team_id]

-- | List teams for current authenticated user
--
-- > listTeamsCurrent' (GithubOAuth "token")
listTeamsCurrent' :: GithubAuth -> IO (Either Error [DetailedTeam])
listTeamsCurrent' auth = githubGet' (Just auth) ["user", "teams"]
