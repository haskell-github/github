module Github.PullRequests (
 pullRequestsFor
,pullRequest
,module Github.Data
) where

import Github.Data
import Github.Private

pullRequestsFor :: String -> String -> IO (Either Error [PullRequest])
pullRequestsFor userName repoName =
  githubGet ["repos", userName, repoName, "pulls"]

pullRequest :: String -> String -> Int -> IO (Either Error DetailedPullRequest)
pullRequest userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number]
