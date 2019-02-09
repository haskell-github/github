-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module GitHub.Endpoints.PullRequests.Comments (
    pullRequestCommentsIO,
    pullRequestCommentsR,
    pullRequestComment,
    pullRequestCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the comments on a pull request with the given ID.
--
-- > pullRequestComments "thoughtbot" "factory_girl" (Id 256)
pullRequestCommentsIO :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector Comment))
pullRequestCommentsIO user repo prid =
    executeRequest' $ pullRequestCommentsR user repo prid FetchAll

-- | List comments on a pull request.
-- See <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
pullRequestCommentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector Comment)
pullRequestCommentsR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "comments"] []

-- | One comment on a pull request, by the comment's ID.
--
-- > pullRequestComment "thoughtbot" "factory_girl" (Id 301819)
pullRequestComment :: Name Owner -> Name Repo -> Id Comment -> IO (Either Error Comment)
pullRequestComment user repo cid =
    executeRequest' $ pullRequestCommentR user repo cid

-- | Query a single comment.
-- See <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
pullRequestCommentR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
pullRequestCommentR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "pulls", "comments", toPathPart cid] []
