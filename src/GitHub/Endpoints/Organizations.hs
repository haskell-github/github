-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The orgs API as described on <http://developer.github.com/v3/orgs/>.
module GitHub.Endpoints.Organizations (
    publicOrganizationsForR,
    publicOrganizationR,
    organizationsR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List all user organizations.
-- See <https://developer.github.com/v3/orgs/#list-your-organizations>
organizationsR :: FetchCount -> Request k (Vector SimpleOrganization)
organizationsR = pagedQuery ["user", "orgs"] []

-- | List public user organizations.
-- See <https://developer.github.com/v3/orgs/#list-user-organizations>
publicOrganizationsForR :: Name User -> FetchCount -> Request k (Vector SimpleOrganization)
publicOrganizationsForR user = pagedQuery ["users", toPathPart user, "orgs"] []

-- | Query an organization.
-- See <https://developer.github.com/v3/orgs/#get-an-organization>
publicOrganizationR :: Name Organization -> Request k Organization
publicOrganizationR reqOrganizationName = query ["orgs", toPathPart reqOrganizationName] []
