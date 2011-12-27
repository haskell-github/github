module Github.Users.Followers (
 usersFollowing
,usersFollowedBy
,module Github.Data
) where

import Github.Data
import Github.Private

usersFollowing :: String -> IO (Either Error [GithubUser])
usersFollowing userName = githubGet ["users", userName, "followers"]

usersFollowedBy :: String -> IO (Either Error [GithubUser])
usersFollowedBy userName = githubGet ["users", userName, "following"]
