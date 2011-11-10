module Github.Gists (
 gists
,module Github.Data
) where

import Github.Data
import Github.Private

gists :: String -> IO (Either Error [Gist])
gists userName = githubGet ["users", userName, "gists"]
