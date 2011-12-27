-- | The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.
module Github.Users (
 userInfoFor
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: String -> IO (Either Error DetailedUser)
userInfoFor userName = githubGet ["users", userName]
