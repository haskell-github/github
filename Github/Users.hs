module Github.Users (
 userInfoFor
,module Github.Data
) where

import Github.Data
import Github.Private

userInfoFor :: String -> IO (Either Error DetailedUser)
userInfoFor userName = githubGet ["users", userName]
