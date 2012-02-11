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
-- > usersFollowing def "mike-burns"
usersFollowing :: GithubConfig -> String -> IO (Either Error [GithubOwner])
usersFollowing c userName = githubGet c ["users", userName, "followers"]

-- | All the users that the given user follows.
--
-- > usersFollowedBy def "mike-burns"
usersFollowedBy :: GithubConfig -> String -> IO (Either Error [GithubOwner])
usersFollowedBy c userName = githubGet c ["users", userName, "following"]
