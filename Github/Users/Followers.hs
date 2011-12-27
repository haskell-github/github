module Github.Users.Followers (
 usersFollowing
,module Github.Data
) where

import Github.Data
import Github.Private

usersFollowing :: String -> IO (Either Error [GithubUser])
usersFollowing userName = githubGet ["users", userName, "followers"]
