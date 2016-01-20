-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module GitHub.Endpoints.Organizations.Members (
    membersOf,
    membersOf',
    membersOfR,
    module GitHub.Data,
    ) where

import Data.Vector (Vector)

import GitHub.Data
import GitHub.Request

-- | All the users who are members of the specified organization,
-- | with or without authentication.
--
-- > membersOf' (Just $ GithubOAuth "token") "thoughtbot"
membersOf' :: Maybe GithubAuth -> Name Organization -> IO (Either Error (Vector SimpleUser))
membersOf' auth org =
    executeRequestMaybe auth $ membersOfR org Nothing

-- | All the users who are members of the specified organization,
-- | without authentication.
--
-- > membersOf "thoughtbot"
membersOf :: Name Organization -> IO (Either Error (Vector SimpleUser))
membersOf = membersOf' Nothing

-- | All the users who are members of the specified organization.
--
-- See <https://developer.github.com/v3/orgs/members/#members-list>
membersOfR :: Name Organization -> Maybe Count -> GithubRequest k (Vector SimpleUser)
membersOfR organization = GithubPagedGet ["orgs", toPathPart organization, "members"] []
