-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.
module GitHub.Endpoints.Users.Followers (
    usersFollowing,
    usersFollowedBy,
    usersFollowingR,
    usersFollowedByR,
    module GitHub.Data,
    ) where

import Data.Vector (Vector)

import GitHub.Data
import GitHub.Request

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowing user =
    executeRequest' $ usersFollowingR user Nothing

-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name User -> Maybe Count -> Request k (Vector SimpleUser)
usersFollowingR user = PagedQuery ["users", toPathPart user, "followers"] []

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowedBy user =
    executeRequest' $ usersFollowedByR user Nothing

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name User -> Maybe Count -> Request k (Vector SimpleUser)
usersFollowedByR user = PagedQuery ["users", toPathPart user, "following"] []
