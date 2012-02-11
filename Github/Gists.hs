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
-- > gists def "mike-burns"
gists :: GithubConfig -> String -> IO (Either Error [Gist])
gists c userName = githubGet c ["users", userName, "gists"]

-- | A specific gist, given its id.
--
-- > gist def "225074"
gist :: GithubConfig -> String -> IO (Either Error Gist)
gist c gistId = githubGet c ["gists", gistId]
