-- | The orgs API as described on <http://developer.github.com/v3/orgs/>.
module Github.Organizations (
 publicOrganizationsFor
,publicOrganizationsFor'
,publicOrganization
,publicOrganization'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The public organizations for a user, given the user's login, with authorization
--
-- > publicOrganizationsFor' (Just ("github-username", "github-password")) "mike-burns"
publicOrganizationsFor' :: Maybe GithubAuth -> String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor' auth userName = githubGet' auth ["users", userName, "orgs"]

-- | The public organizations for a user, given the user's login.
--
-- > publicOrganizationsFor "mike-burns"
publicOrganizationsFor :: String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor = publicOrganizationsFor' Nothing

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization' (Just ("github-username", "github-password")) "thoughtbot"
publicOrganization' :: Maybe GithubAuth -> String -> IO (Either Error Organization)
publicOrganization' auth reqOrganizationName = githubGet' auth ["orgs", reqOrganizationName]

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization "thoughtbot"
publicOrganization :: String -> IO (Either Error Organization)
publicOrganization = publicOrganization' Nothing
