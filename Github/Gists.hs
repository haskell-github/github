-- | The gists API as described at <http://developer.github.com/v3/gists/>.
module Github.Gists (
 gists
,gist
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of all public gists created by the user.
--
-- > gists "mike-burns"
gists :: String -> IO (Either Error [Gist])
gists userName = githubGet ["users", userName, "gists"]

-- | A specific gist, given its id.
--
-- > gist "225074"
gist :: String -> IO (Either Error Gist)
gist gistId = githubGet ["gists", gistId]
