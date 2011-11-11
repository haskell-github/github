module Github.Issues (
 issue
,module Github.Data
) where

import Github.Data
import Github.Private

issue :: String -> String -> Int -> IO (Either Error Issue)
issue user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber]

