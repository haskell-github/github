module Github.Issues.Comments (
 comment
,comments
,module Github.Data
) where

import Github.Data
import Github.Private

comment :: String -> String -> Int -> IO (Either Error IssueComment)
comment user repoName commentId =
  githubGet ["repos", user, repoName, "issues", "comments", show commentId]

comments :: String -> String -> Int -> IO (Either Error [IssueComment])
comments user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber, "comments"]
