module Github.Issues.Labels (
 repoLabels
,module Github.Data
) where

import Github.Data
import Github.Private

repoLabels :: String -> String -> IO (Either Error [IssueLabel])
repoLabels user repoName = githubGet ["repos", user, repoName, "labels"]
