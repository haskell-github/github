-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module GitHub.Endpoints.PullRequests.ReviewComments (
    pullRequestReviewCommentsIO,
    pullRequestReviewCommentsR,
    pullRequestReviewComment,
    pullRequestReviewCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Request
import GitHub.Internal.Prelude

-- | All the comments on a pull request with the given ID.
--
-- > pullRequestReviewComments "thoughtbot" "factory_girl" (Id 256)
pullRequestReviewCommentsIO :: Name Owner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector Comment))
pullRequestReviewCommentsIO user repo prid =
    executeRequest' $ pullRequestReviewCommentsR user repo prid FetchAll

-- | List comments on a pull request.
-- See <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
pullRequestReviewCommentsR :: Name Owner -> Name Repo -> Id PullRequest -> FetchCount -> Request k (Vector Comment)
pullRequestReviewCommentsR user repo prid =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "comments"] []

-- | One comment on a pull request, by the comment's ID.
--
-- > pullRequestReviewComment "thoughtbot" "factory_girl" (Id 301819)
pullRequestReviewComment :: Name Owner -> Name Repo -> Id Comment -> IO (Either Error Comment)
pullRequestReviewComment user repo cid =
    executeRequest' $ pullRequestReviewCommentR user repo cid

-- | Query a single comment.
-- See <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
pullRequestReviewCommentR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
pullRequestReviewCommentR user repo cid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", "comments", toPathPart cid] []
