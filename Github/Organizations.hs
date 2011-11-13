module Github.Organizations (
 publicOrganizationsFor
,publicOrganization
,module Github.Data
) where

import Github.Data
import Github.Private

publicOrganizationsFor :: String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor userName = githubGet ["users", userName, "orgs"]

publicOrganization :: String -> IO (Either Error Organization)
publicOrganization organizationName = githubGet ["orgs", organizationName]
