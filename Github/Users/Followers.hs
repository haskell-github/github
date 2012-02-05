-- | The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.
module Github.Users.Followers (
 usersFollowing
,usersFollowedBy
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: String -> IO (Either Error [GithubOwner])
usersFollowing userName = githubGet ["users", userName, "followers"]

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: String -> IO (Either Error [GithubOwner])
usersFollowedBy userName = githubGet ["users", userName, "following"]
