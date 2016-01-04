-- | The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.
module Github.Users.Followers (
    usersFollowing,
    usersFollowedBy,
    usersFollowingR,
    usersFollowedByR,
    module Github.Data,
    ) where

import Github.Data
import Github.Request

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: Name GithubOwner -> IO (Either Error [GithubOwner])
usersFollowing = executeRequest' . usersFollowingR

-- | List followers of a user. All the users following the given user.
--
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name GithubOwner -> GithubRequest k [GithubOwner]
usersFollowingR userName = GithubGet ["users", untagName userName, "followers"] "" -- TODO: use paged get

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name GithubOwner -> IO (Either Error [GithubOwner])
usersFollowedBy = executeRequest' . usersFollowedByR

-- | List users followed by another user. All the users that the given user follows.
--
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name GithubOwner -> GithubRequest k [GithubOwner]
usersFollowedByR userName = GithubGet ["users", untagName userName, "following"] "" -- TODO: use paged get
