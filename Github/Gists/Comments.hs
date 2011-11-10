module Github.Gists.Comments (
 commentsOn
,comment
,module Github.Data
) where

import Github.Data
import Github.Private

commentsOn :: String -> IO (Either Error [GistComment])
commentsOn gistId = githubGet ["gists", gistId, "comments"]

comment :: String -> IO (Either Error GistComment)
comment commentId = githubGet ["gists", "comments", commentId]
