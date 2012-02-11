-- | The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module Github.PullRequests.ReviewComments (
 pullRequestReviewComments
,pullRequestReviewComment
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the comments on a pull request with the given ID.
--
-- > pullRequestReviewComments def "thoughtbot" "factory_girl" 256
pullRequestReviewComments :: GithubConfig -> String -> String -> Int -> IO (Either Error [Comment])
pullRequestReviewComments c userName repoName number =
  githubGet c ["repos", userName, repoName, "pulls", show number, "comments"]

-- | One comment on a pull request, by the comment's ID.
--
-- > pullRequestReviewComment def "thoughtbot" "factory_girl" 301819
pullRequestReviewComment :: GithubConfig -> String -> String -> Int -> IO (Either Error Comment)
pullRequestReviewComment c userName repoName id =
  githubGet c ["repos", userName, repoName, "pulls", "comments", show id]
