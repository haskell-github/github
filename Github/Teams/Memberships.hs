module Github.Teams.Memberships (
 teamMembershipInfoFor
,teamMembershipInfoFor'
,addTeamMembershipFor'
,deleteTeamMembershipFor'
, module Github.Data
) where

import Github.Data
import Github.Private

-- | Retrieve team mebership information for a user.
-- | With authentication
--
-- > teamMembershipInfoFor' (Just $ GithubOAuth "token") 1010101 "mburns"
teamMembershipInfoFor' :: Maybe GithubAuth -> Int -> String -> IO (Either Error TeamMembership)
teamMembershipInfoFor' auth team_id username =
  githubGet' auth ["teams", show team_id, "memberships", username]

-- | Retrieve team mebership information for a user.
--
-- > teamMembershipInfoFor 1010101 "mburns"
teamMembershipInfoFor :: Int -> String -> IO (Either Error TeamMembership)
teamMembershipInfoFor = teamMembershipInfoFor' Nothing

-- | Add (or invite) a member to a team.
--
-- > addTeamMembershipFor' (GithubOAuth "token") 1010101 "mburns" RoleMember
addTeamMembershipFor' :: GithubAuth -> Int -> String -> Role -> IO (Either Error TeamMembership)
addTeamMembershipFor' auth team_id username role =
  githubPut auth ["teams", show team_id, "memberships", username] (CreateTeamMembership role)

-- | Delete a member of a team.
--
-- > deleteTeamMembershipFor' (GithubOAuth "token") 1010101 "mburns"
deleteTeamMembershipFor' :: GithubAuth -> Int -> String -> IO (Either Error ())
deleteTeamMembershipFor' auth team_id username =
  githubDelete auth ["teams", show team_id, "memberships", username]
