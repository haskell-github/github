module Github.Issues.Labels (
 repoLabels
,label
,module Github.Data
) where

import Github.Data
import Github.Private

repoLabels :: String -> String -> IO (Either Error [IssueLabel])
repoLabels user repoName = githubGet ["repos", user, repoName, "labels"]

label :: String -> String -> String -> IO (Either Error IssueLabel)
label user repoName labelName =
  githubGet ["repos", user, repoName, "labels", labelName]
