-- |
-- The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.

module GitHub.Endpoints.Users.Followers (
    usersFollowingR,
    usersFollowedByR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name User -> FetchCount -> Request k (Vector SimpleUser)
usersFollowingR user =
    pagedQuery ["users", toPathPart user, "followers"] []

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name User -> FetchCount -> Request k (Vector SimpleUser)
usersFollowedByR user =
    pagedQuery ["users", toPathPart user, "following"] []
