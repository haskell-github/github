-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module GitHub.Endpoints.PullRequests.Comments (
    pullRequestCommentsR,
    pullRequestCommentR,
    createPullCommentR,
    createPullCommentReplyR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List comments on a pull request.
-- See <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
pullRequestCommentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector Comment)
pullRequestCommentsR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "comments"] []

-- | Query a single comment.
-- See <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
pullRequestCommentR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
pullRequestCommentR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "pulls", "comments", toPathPart cid] []

-- | Create a comment.
--
-- See <https://developer.github.com/v3/pulls/comments/#create-a-comment>
createPullCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Text -> Int -> Text -> Request 'RW Comment
createPullCommentR user repo iss commit path position body =
    command Post parts (encode $ NewPullComment commit path position body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart iss, "comments"]

-- | Create a comment reply.
--
-- See <https://developer.github.com/v3/pulls/comments/#create-a-review-comment-reply>
createPullCommentReplyR :: Name Owner -> Name Repo -> IssueNumber -> Id Comment -> Text -> Request 'RW Comment
createPullCommentReplyR user repo iss cid body =
    command Post parts (encode $ PullCommentReply body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart iss
            , "comments", toPathPart cid, "replies"]
