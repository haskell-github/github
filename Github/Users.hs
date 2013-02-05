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
-- > userInfoFor' (Just ("github-username", "github-password")) "mike-burns"
userInfoFor' :: Maybe GithubAuth -> String -> IO (Either Error DetailedOwner)
userInfoFor' auth userName = githubGet' auth ["users", userName]

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: String -> IO (Either Error DetailedOwner)
userInfoFor = userInfoFor' Nothing
