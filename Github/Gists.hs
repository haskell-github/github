module Github.Gists (
 gists
,gist
,module Github.Data
) where

import Github.Data
import Github.Private

gists :: String -> IO (Either Error [Gist])
gists userName = githubGet ["users", userName, "gists"]

gist :: String -> IO (Either Error Gist)
gist gistId = githubGet ["gists", gistId]
