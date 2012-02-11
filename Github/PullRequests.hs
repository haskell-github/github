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
-- > pullRequestsFor def "rails" "rails"
pullRequestsFor :: GithubConfig -> String -> String -> IO (Either Error [PullRequest])
pullRequestsFor c userName repoName =
  githubGet c ["repos", userName, repoName, "pulls"]

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
--
-- > pullRequest def "thoughtbot" "paperclip" 562
pullRequest :: GithubConfig -> String -> String -> Int -> IO (Either Error DetailedPullRequest)
pullRequest c userName repoName number =
  githubGet c ["repos", userName, repoName, "pulls", show number]

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
--
-- > pullRequestCommits def "thoughtbot" "paperclip" 688
pullRequestCommits :: GithubConfig -> String -> String -> Int -> IO (Either Error [Commit])
pullRequestCommits c userName repoName number =
  githubGet c ["repos", userName, repoName, "pulls", show number, "commits"]

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
--
-- > pullRequestFiles def "thoughtbot" "paperclip" 688
pullRequestFiles :: GithubConfig -> String -> String -> Int -> IO (Either Error [File])
pullRequestFiles c userName repoName number =
  githubGet c ["repos", userName, repoName, "pulls", show number, "files"]
