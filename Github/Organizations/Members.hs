-- | The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module Github.Organizations.Members (
    membersOf,
    membersOf',
    membersOfR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Auth
import Github.Data
import Github.Request

-- | All the users who are members of the specified organization,
-- | with or without authentication.
--
-- > membersOf' (Just $ GithubOAuth "token") "thoughtbot"
membersOf' :: Maybe GithubAuth -> Name Organization -> IO (Either Error (Vector SimpleOwner))
membersOf' auth org =
    executeRequestMaybe auth $ membersOfR org Nothing

-- | All the users who are members of the specified organization,
-- | without authentication.
--
-- > membersOf "thoughtbot"
membersOf :: Name Organization -> IO (Either Error (Vector SimpleOwner))
membersOf = membersOf' Nothing

-- | All the users who are members of the specified organization.
--
-- See <https://developer.github.com/v3/orgs/members/#members-list>
membersOfR :: Name Organization -> Maybe Count -> GithubRequest k (Vector SimpleOwner)
membersOfR organization = GithubPagedGet ["orgs", untagName organization, "members"] []
