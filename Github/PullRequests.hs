-- | The pull requests API as documented at
-- <http://developer.github.com/v3/pulls/>.
module Github.PullRequests (
 pullRequestsFor
,pullRequest
,pullRequestCommits
,pullRequestFiles
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All pull requests for the repo, by owner and repo name.
--
-- > pullRequestsFor "rails" "rails"
pullRequestsFor :: String -> String -> IO (Either Error [PullRequest])
pullRequestsFor userName repoName =
  githubGet ["repos", userName, repoName, "pulls"]

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
--
-- > pullRequest "thoughtbot" "paperclip" 562
pullRequest :: String -> String -> Int -> IO (Either Error DetailedPullRequest)
pullRequest userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number]

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
--
-- > pullRequestCommits "thoughtbot" "paperclip" 688
pullRequestCommits :: String -> String -> Int -> IO (Either Error [Commit])
pullRequestCommits userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number, "commits"]

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
--
-- > pullRequestFiles "thoughtbot" "paperclip" 688
pullRequestFiles :: String -> String -> Int -> IO (Either Error [File])
pullRequestFiles userName repoName number =
  githubGet ["repos", userName, repoName, "pulls", show number, "files"]
