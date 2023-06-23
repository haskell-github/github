-- |
-- The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.

module GitHub.Endpoints.Issues.Comments (
    commentR,
    commentsR,
    createCommentR,
    deleteCommentR,
    editCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Query a single comment.
-- See <https://developer.github.com/v3/issues/comments/#get-a-single-comment>
commentR :: Name Owner -> Name Repo -> Id Comment -> Request k IssueComment
commentR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart cid] []

-- | List comments on an issue.
-- See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>
commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
commentsR user repo iid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "comments"] []

-- | Create a comment.
-- See <https://developer.github.com/v3/issues/comments/#create-a-comment>
createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request 'RW Comment
createCommentR user repo iss body =
    command Post parts (encode $ NewComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss, "comments"]

-- | Edit a comment.
-- See <https://developer.github.com/v3/issues/comments/#edit-a-comment>
editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request 'RW Comment
editCommentR user repo commid body =
    command Patch parts (encode $ EditComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart commid]

-- | Delete a comment.
-- See <https://developer.github.com/v3/issues/comments/#delete-a-comment>
deleteCommentR :: Name Owner -> Name Repo -> Id Comment -> GenRequest 'MtUnit 'RW ()
deleteCommentR user repo commid =
    Command Delete parts mempty
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart commid]
