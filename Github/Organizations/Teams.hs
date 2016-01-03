-- | The organization teams API as described on
-- <http://developer.github.com/v3/orgs/teams/>.
module Github.Organizations.Teams (
    teamsOf,
    teamsOf',
    teamsOfR,
    module Github.Data,
    ) where

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

-- | List teams.  List the teams of an organization.
-- When authenticated, lists private teams visible to the authenticated user.
-- When unauthenticated, lists only public teams for an organization.
--
-- See <https://developer.github.com/v3/orgs/teams/#list-teams>
teamsOfR :: Name Organization -> GithubRequest k [Team]
teamsOfR organization = GithubGet ["orgs", untagName organization, "teams"] ""
