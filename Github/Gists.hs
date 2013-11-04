-- | The gists API as described at <http://developer.github.com/v3/gists/>.
module Github.Gists (
 gists
,gists'
,gist
,gist'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of all gists created by the user 
-- 
-- > gists' (Just ("github-username", "github-password")) "mike-burns"
gists' :: Maybe GithubAuth -> String -> IO (Either Error [Gist])
gists' auth userName = githubGet' auth ["users", userName, "gists"]

-- | The list of all public gists created by the user.
--
-- > gists "mike-burns"
gists :: String -> IO (Either Error [Gist])
gists = gists' Nothing

-- | A specific gist, given its id, with authentication credentials
--
-- > gist' (Just ("github-username", "github-password")) "225074"
gist' :: Maybe GithubAuth -> String -> IO (Either Error Gist)
gist' auth reqGistId = githubGet' auth ["gists", reqGistId]

-- | A specific gist, given its id.
--
-- > gist "225074"
gist :: String -> IO (Either Error Gist)
gist = gist' Nothing
