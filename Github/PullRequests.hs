module Github.PullRequests (
 pullRequestsFor
,pullRequest
,pullRequestCommits
,pullRequestFiles
,pullRequestReviewComments
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

pullRequestCommits :: String -> String -> Int -> IO (Either Error [Commit])
pullRequestCommits userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number, "commits"]

pullRequestFiles :: String -> String -> Int -> IO (Either Error [File])
pullRequestFiles userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number, "files"]

pullRequestReviewComments :: String -> String -> Int -> IO (Either Error [Comment])
pullRequestReviewComments userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number, "comments"]
