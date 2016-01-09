-- | The user followers API as described on
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
usersFollowing :: Name GithubOwner -> IO (Either Error (Vector GithubOwner))
usersFollowing user =
    executeRequest' $ usersFollowingR user Nothing

-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector GithubOwner)
usersFollowingR userName = GithubPagedGet ["users", untagName userName, "followers"] []

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name GithubOwner -> IO (Either Error (Vector GithubOwner))
usersFollowedBy user =
    executeRequest' $ usersFollowedByR user Nothing

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector GithubOwner)
usersFollowedByR userName = GithubPagedGet ["users", untagName userName, "following"] []
