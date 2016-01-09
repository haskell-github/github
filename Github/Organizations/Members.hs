-- | The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module Github.Organizations.Members (
    membersOf,
    membersOf',
    membersOfR,
    module Github.Data,
    ) where

import Github.Auth
import Github.Data
import Github.Request
import Data.Vector (Vector)

-- | All the users who are members of the specified organization,
-- | with or without authentication.
--
-- > membersOf' (Just $ GithubOAuth "token") "thoughtbot"
membersOf' :: Maybe GithubAuth -> Name Organization -> IO (Either Error (Vector GithubOwner))
membersOf' auth org =
    executeRequestMaybe auth $ membersOfR org Nothing

-- | All the users who are members of the specified organization,
-- | without authentication.
--
-- > membersOf "thoughtbot"
membersOf :: Name Organization -> IO (Either Error (Vector GithubOwner))
membersOf = membersOf' Nothing

-- | All the users who are members of the specified organization.
--
-- See <https://developer.github.com/v3/orgs/members/#members-list>
membersOfR :: Name Organization -> Maybe Count -> GithubRequest k (Vector GithubOwner)
membersOfR organization = GithubPagedGet ["orgs", untagName organization, "members"] []
