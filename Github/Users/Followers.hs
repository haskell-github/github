-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.
module Github.Users.Followers (
    usersFollowing,
    usersFollowedBy,
    usersFollowingR,
    usersFollowedByR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: Name GithubOwner -> IO (Either Error (Vector SimpleOwner))
usersFollowing user =
    executeRequest' $ usersFollowingR user Nothing

-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector SimpleOwner)
usersFollowingR userName = GithubPagedGet ["users", toPathPart userName, "followers"] []

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name GithubOwner -> IO (Either Error (Vector SimpleOwner))
usersFollowedBy user =
    executeRequest' $ usersFollowedByR user Nothing

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector SimpleOwner)
usersFollowedByR userName = GithubPagedGet ["users", toPathPart userName, "following"] []
