-- | The orgs API as described on <http://developer.github.com/v3/orgs/>.
module Github.Organizations (
 publicOrganizationsFor
,publicOrganization
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The public organizations for a user, given the user's login.
--
-- > publicOrganizationsFor "mike-burns"
publicOrganizationsFor :: String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor userName = githubGet ["users", userName, "orgs"]

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization "thoughtbot"
publicOrganization :: String -> IO (Either Error Organization)
publicOrganization organizationName = githubGet ["orgs", organizationName]
