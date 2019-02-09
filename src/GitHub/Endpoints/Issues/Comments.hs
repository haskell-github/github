-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module GitHub.Endpoints.Issues.Comments (
    comment,
    commentR,
    comments,
    commentsR,
    comments',
    createComment,
    createCommentR,
    deleteComment,
    deleteCommentR,
    editComment,
    editCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | A specific comment, by ID.
--
-- > comment "thoughtbot" "paperclip" 1468184
comment :: Name Owner -> Name Repo -> Id Comment -> IO (Either Error IssueComment)
comment user repo cid =
    executeRequest' $ commentR user repo cid

-- | Query a single comment.
-- See <https://developer.github.com/v3/issues/comments/#get-a-single-comment>
commentR :: Name Owner -> Name Repo -> Id Comment -> Request k IssueComment
commentR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart cid] []

-- | All comments on an issue, by the issue's number.
--
-- > comments "thoughtbot" "paperclip" 635
comments :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector IssueComment))
comments = comments' Nothing

-- | All comments on an issue, by the issue's number, using authentication.
--
-- > comments' (User (user, password)) "thoughtbot" "paperclip" 635
comments' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector IssueComment))
comments' auth user repo iid =
    executeRequestMaybe auth $ commentsR user repo iid FetchAll

-- | List comments on an issue.
-- See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>
commentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector IssueComment)
commentsR user repo iid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "comments"] []

-- | Create a new comment.
--
-- > createComment (User (user, password)) user repo issue
-- >  "some words"
createComment :: Auth -> Name Owner -> Name Repo -> IssueNumber -> Text
            -> IO (Either Error Comment)
createComment auth user repo iss body =
    executeRequest auth $ createCommentR user repo iss body

-- | Create a comment.
-- See <https://developer.github.com/v3/issues/comments/#create-a-comment>
createCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Request 'RW Comment
createCommentR user repo iss body =
    command Post parts (encode $ NewComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss, "comments"]

-- | Edit a comment.
--
-- > editComment (User (user, password)) user repo commentid
-- >  "new words"
editComment :: Auth -> Name Owner -> Name Repo -> Id Comment -> Text
            -> IO (Either Error Comment)
editComment auth user repo commid body =
    executeRequest auth $ editCommentR user repo commid body

-- | Edit a comment.
-- See <https://developer.github.com/v3/issues/comments/#edit-a-comment>
editCommentR :: Name Owner -> Name Repo -> Id Comment -> Text -> Request 'RW Comment
editCommentR user repo commid body =
    command Patch parts (encode $ EditComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart commid]

-- | Delete a comment.
--
-- > deleteComment (User (user, password)) user repo commentid
deleteComment :: Auth -> Name Owner -> Name Repo -> Id Comment
              -> IO (Either Error ())
deleteComment auth user repo commid =
    executeRequest auth $ deleteCommentR user repo commid

-- | Delete a comment.
-- See <https://developer.github.com/v3/issues/comments/#delete-a-comment>
deleteCommentR :: Name Owner -> Name Repo -> Id Comment -> Request 'RW ()
deleteCommentR user repo commid =
    command Delete parts mempty
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart commid]
