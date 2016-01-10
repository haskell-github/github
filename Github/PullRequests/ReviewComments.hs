-- | The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module Github.PullRequests.ReviewComments (
    pullRequestReviewCommentsIO,
    pullRequestReviewCommentsR,
    pullRequestReviewComment,
    pullRequestReviewCommentR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | All the comments on a pull request with the given ID.
--
-- > pullRequestReviewComments "thoughtbot" "factory_girl" (Id 256)
pullRequestReviewCommentsIO :: Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector Comment))
pullRequestReviewCommentsIO user repo prid =
    executeRequest' $ pullRequestReviewCommentsR user repo prid Nothing

-- | List comments on a pull request.
-- See <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
pullRequestReviewCommentsR :: Name GithubOwner -> Name Repo -> Id PullRequest -> Maybe Count -> GithubRequest k (Vector Comment)
pullRequestReviewCommentsR user repo prid =
    GithubPagedGet ["repos", untagName user, untagName repo, "pulls", show $ untagId prid, "comments"] []

-- | One comment on a pull request, by the comment's ID.
--
-- > pullRequestReviewComment "thoughtbot" "factory_girl" (Id 301819)
pullRequestReviewComment :: Name GithubOwner -> Name Repo -> Id Comment -> IO (Either Error Comment)
pullRequestReviewComment user repo cid =
    executeRequest' $ pullRequestReviewCommentR user repo cid

-- | Get a single comment.
-- See <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
pullRequestReviewCommentR :: Name GithubOwner -> Name Repo -> Id Comment -> GithubRequest k Comment
pullRequestReviewCommentR user repo cid =
    GithubGet ["repos", untagName user, untagName repo, "pulls", "comments", show $ untagId cid] []
