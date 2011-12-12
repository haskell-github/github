module Github.PullRequests (
pullRequestsFor
,module Github.Data
) where

import Github.Data
import Github.Private

pullRequestsFor :: String -> String -> IO (Either Error [PullRequest])
pullRequestsFor userName repoName =
  githubGet ["repos", userName, repoName, "pulls"]
