{-# LANGUAGE DataKinds #-}
-- | The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module Github.Issues.Comments (
    comment,
    commentR,
    comments,
    commentsR,
    comments',
    createComment,
    createCommentR,
    editComment,
    editCommentR,
    module Github.Data,
    ) where

import Data.Aeson.Compat (encode)
import Data.Text         (Text)
import Data.Vector       (Vector)

import Github.Data
import Github.Request

-- | A specific comment, by ID.
--
-- > comment "thoughtbot" "paperclip" 1468184
comment :: Name GithubOwner -> Name Repo -> Id Comment -> IO (Either Error IssueComment)
comment user repo cid =
    executeRequest' $ commentR user repo cid

-- | Get a single comment.
-- See <https://developer.github.com/v3/issues/comments/#get-a-single-comment>
commentR :: Name GithubOwner -> Name Repo -> Id Comment -> GithubRequest k IssueComment
commentR user repo cid =
    GithubGet ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart cid] []

-- | All comments on an issue, by the issue's number.
--
-- > comments "thoughtbot" "paperclip" 635
comments :: Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueComment))
comments = comments' Nothing

-- | All comments on an issue, by the issue's number, using authentication.
--
-- > comments' (GithubUser (user, password)) "thoughtbot" "paperclip" 635
comments' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueComment))
comments' auth user repo iid =
    executeRequestMaybe auth $ commentsR user repo iid Nothing

-- | List comments on an issue.
-- See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>
commentsR :: Name GithubOwner -> Name Repo -> Id Issue -> Maybe Count -> GithubRequest k (Vector IssueComment)
commentsR user repo iid =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "comments"] []

-- | Create a new comment.
--
-- > createComment (GithubUser (user, password)) user repo issue
-- >  "some words"
createComment :: GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> Text
            -> IO (Either Error Comment)
createComment auth user repo iss body =
    executeRequest auth $ createCommentR user repo iss body

-- | Create a comment.
-- See <https://developer.github.com/v3/issues/comments/#create-a-comment>
createCommentR :: Name GithubOwner -> Name Repo -> Id Issue -> Text -> GithubRequest 'True Comment
createCommentR user repo iss body =
    GithubPost Post parts (encode $ NewComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss, "comments"]

-- | Edit a comment.
--
-- > editComment (GithubUser (user, password)) user repo commentid
-- >  "new words"
editComment :: GithubAuth -> Name GithubOwner -> Name Repo -> Id Comment -> Text
            -> IO (Either Error Comment)
editComment auth user repo commid body =
    executeRequest auth $ editCommentR user repo commid body

-- | Edit a comment.
-- See <https://developer.github.com/v3/issues/comments/#edit-a-comment>
editCommentR :: Name GithubOwner -> Name Repo -> Id Comment -> Text -> GithubRequest 'True Comment
editCommentR user repo commid body =
    GithubPost Patch parts (encode $ EditComment body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "issues", "comments", toPathPart commid]
