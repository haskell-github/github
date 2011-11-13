module Github.Organizations (
 publicOrganizationsFor
,module Github.Data
) where

import Github.Data
import Github.Private

publicOrganizationsFor :: String -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor userName = githubGet ["users", userName, "orgs"]
