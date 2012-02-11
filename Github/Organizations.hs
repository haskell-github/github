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
-- > publicOrganizationsFor def "mike-burns"
publicOrganizationsFor :: GithubConfig -> String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor c userName = githubGet c ["users", userName, "orgs"]

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization def "thoughtbot"
publicOrganization :: GithubConfig -> String -> IO (Either Error Organization)
publicOrganization c organizationName =
  githubGet c ["orgs", organizationName]
