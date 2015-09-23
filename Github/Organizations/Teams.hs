-- | The organization teams API as described on
-- <http://developer.github.com/v3/orgs/teams/>.
module Github.Organizations.Teams (
 teamsOf
,teamsOf'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | List the teams of an organization.
-- | When authenticated, lists private teams visible to the authenticated user.
-- | When unauthenticated, lists only public teams for an organization.
--
-- > teamsOf' (Just $ GithubOAuth "token") "thoughtbot"
teamsOf' :: Maybe GithubAuth -> String -> IO (Either Error [Team])
teamsOf' auth organization = githubGet' auth ["orgs", organization, "teams"]

-- | List the public teams of an organization.
--
-- > teamsOf "thoughtbot"
teamsOf :: String -> IO (Either Error [Team])
teamsOf = teamsOf' Nothing
