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

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | The public organizations for a user, given the user's login, with authorization
--
-- > publicOrganizationsFor' (Just ("github-username", "github-password")) "mike-burns"
publicOrganizationsFor' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error (Vector SimpleOrganization))
publicOrganizationsFor' auth org =
    executeRequestMaybe auth $ publicOrganizationsForR org Nothing

-- | List user organizations. The public organizations for a user, given the user's login.
--
-- > publicOrganizationsFor "mike-burns"
publicOrganizationsFor :: Name GithubOwner -> IO (Either Error (Vector SimpleOrganization))
publicOrganizationsFor = publicOrganizationsFor' Nothing

-- | List user organizations.
-- See <https://developer.github.com/v3/orgs/#list-user-organizations>
publicOrganizationsForR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector SimpleOrganization)
publicOrganizationsForR userName = GithubPagedGet ["users", toPathPart userName, "orgs"] []

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

-- | Get an organization.
-- See <https://developer.github.com/v3/orgs/#get-an-organization>
publicOrganizationR :: Name Organization -> GithubRequest k Organization
publicOrganizationR reqOrganizationName = GithubGet ["orgs", toPathPart reqOrganizationName] []
