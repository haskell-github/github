module Github.Issues.Comments (
 comment
,module Github.Data
) where

import Github.Data
import Github.Private

comment :: String -> String -> Int -> IO (Either Error IssueComment)
comment user repoName commentId =
  githubGet ["repos", user, repoName, "issues", "comments", show commentId]
