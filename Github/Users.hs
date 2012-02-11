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
-- > userInfoFor def "mike-burns"
userInfoFor :: GithubConfig -> String -> IO (Either Error DetailedOwner)
userInfoFor c userName = githubGet c ["users", userName]
