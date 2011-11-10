module Github.Gists.Comments (
 commentsOn
,module Github.Data
) where

import Github.Data
import Github.Private

commentsOn :: String -> IO (Either Error [GistComment])
commentsOn gistId = githubGet ["gists", gistId, "comments"]
