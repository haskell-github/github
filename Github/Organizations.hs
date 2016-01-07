-- | The orgs API as described on <http://developer.github.com/v3/orgs/>.
module Github.Organizations (
    publicOrganizationsFor,
    publicOrganizationsFor',
    publicOrganizationsForR,
    publicOrganization,
    publicOrganization',
    publicOrganizationR,
    module Github.Data,
    ) where

import Github.Auth
import Github.Data
import Github.Request

-- | The public organizations for a user, given the user's login, with authorization
--
-- > publicOrganizationsFor' (Just ("github-username", "github-password")) "mike-burns"
publicOrganizationsFor' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor' auth = executeRequestMaybe auth . publicOrganizationsForR

-- | List user organizations. The public organizations for a user, given the user's login.
--
-- > publicOrganizationsFor "mike-burns"
publicOrganizationsFor :: Name GithubOwner -> IO (Either Error [SimpleOrganization])
publicOrganizationsFor = publicOrganizationsFor' Nothing

-- | List user organizations. The public organizations for a user, given the user's login.
--
-- See <https://developer.github.com/v3/orgs/#list-user-organizations>
publicOrganizationsForR :: Name GithubOwner -> GithubRequest k [SimpleOrganization]
publicOrganizationsForR userName = GithubGet ["users", untagName userName, "orgs"] [] -- TODO: Use PagedGet

-- | Details on a public organization. Takes the organization's login.
--
-- > publicOrganization' (Just ("github-username", "github-password")) "thoughtbot"
publicOrganization' :: Maybe GithubAuth -> Name Organization -> IO (Either Error Organization)
publicOrganization' auth = executeRequestMaybe auth . publicOrganizationR

-- | Get an organization. Details on a public organization. Takes the organization's login.
--
-- > publicOrganization "thoughtbot"
publicOrganization :: Name Organization -> IO (Either Error Organization)
publicOrganization = publicOrganization' Nothing

-- | Get an organization. Details on a public organization. Takes the organization's login.
--
-- See <https://developer.github.com/v3/orgs/#get-an-organization>
publicOrganizationR :: Name Organization -> GithubRequest k Organization
publicOrganizationR reqOrganizationName = GithubGet ["orgs", untagName reqOrganizationName] []
