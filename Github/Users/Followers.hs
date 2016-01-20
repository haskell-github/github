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

import Data.Vector (Vector)

import Github.Data
import Github.Request

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowing user =
    executeRequest' $ usersFollowingR user Nothing

-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name User -> Maybe Count -> GithubRequest k (Vector SimpleUser)
usersFollowingR user = GithubPagedGet ["users", toPathPart user, "followers"] []

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowedBy user =
    executeRequest' $ usersFollowedByR user Nothing

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name User -> Maybe Count -> GithubRequest k (Vector SimpleUser)
usersFollowedByR user = GithubPagedGet ["users", toPathPart user, "following"] []
