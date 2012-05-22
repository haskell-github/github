-- | The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.
module Github.Users (
 userInfoFor
,userInfoFor'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The information for a single user, by login name.
-- | With authentification
--
-- > userInfoFor authInfo "mike-burns"
userInfoFor' :: Maybe BasicAuth -> String -> IO (Either Error DetailedOwner)
userInfoFor' auth userName = githubGet' auth ["users", userName]

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: String -> IO (Either Error DetailedOwner)
userInfoFor = userInfoFor' Nothing
