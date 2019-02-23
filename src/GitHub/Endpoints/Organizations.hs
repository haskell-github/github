-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The orgs API as described on <http://developer.github.com/v3/orgs/>.
module GitHub.Endpoints.Organizations (
    publicOrganizationsFor,
    publicOrganizationsFor',
    publicOrganizationsForR,
    publicOrganization,
    publicOrganization',
    publicOrganizationR,
    organizationsR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | The public organizations for a user, given the user's login, with authorization
--
-- > publicOrganizationsFor' (Just ("github-username", "github-password")) "mike-burns"
publicOrganizationsFor' :: Maybe Auth -> Name User -> IO (Either Error (Vector SimpleOrganization))
publicOrganizationsFor' auth org =
    executeRequestMaybe auth $ publicOrganizationsForR org FetchAll

-- | List user organizations. The public organizations for a user, given the user's login.
--
-- > publicOrganizationsFor "mike-burns"
publicOrganizationsFor :: Name User -> IO (Either Error (Vector SimpleOrganization))
publicOrganizationsFor = publicOrganizationsFor' Nothing

-- | List all user organizations.
-- See <https://developer.github.com/v3/orgs/#list-your-organizations>
organizationsR :: FetchCount -> Request k (Vector SimpleOrganization)
organizationsR = pagedQuery ["user", "orgs"] []

-- | List public user organizations.
-- See <https://developer.github.com/v3/orgs/#list-user-organizations>
publicOrganizationsForR :: Name User -> FetchCount -> Request k (Vector SimpleOrganization)
publicOrganizationsForR user = pagedQuery ["users", toPathPart user, "orgs"] []

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization' (Just ("github-username", "github-password")) "thoughtbot"
publicOrganization' :: Maybe Auth -> Name Organization -> IO (Either Error Organization)
publicOrganization' auth = executeRequestMaybe auth . publicOrganizationR

-- | Query an organization. Details on a public organization. Takes the organization's login.
--
-- > publicOrganization "thoughtbot"
publicOrganization :: Name Organization -> IO (Either Error Organization)
publicOrganization = publicOrganization' Nothing

-- | Query an organization.
-- See <https://developer.github.com/v3/orgs/#get-an-organization>
publicOrganizationR :: Name Organization -> Request k Organization
publicOrganizationR reqOrganizationName = query ["orgs", toPathPart reqOrganizationName] []
