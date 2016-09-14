{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo commits API as described on
-- <http://developer.github.com/v3/repos/comments/>.
module GitHub.Endpoints.Repos.Comments (
    commentsFor,
    commentsFor',
    commentsForR,
    commitCommentsFor,
    commitCommentsFor',
    commitCommentsForR,
    commitCommentFor,
    commitCommentFor',
    commitCommentForR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the comments on a Github repo.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor :: Name Owner -> Name Repo -> IO (Either Error (Vector Comment))
commentsFor = commentsFor' Nothing

-- | All the comments on a Github repo.
-- With authentication.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Comment))
commentsFor' auth user repo =
    executeRequestMaybe auth $ commentsForR user repo FetchAll

-- | List commit comments for a repository.
-- See <https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository>
commentsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Comment)
commentsForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "comments"] []

-- | Just the comments on a specific SHA for a given Github repo.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor :: Name Owner -> Name Repo -> Name Commit -> IO (Either Error (Vector Comment))
commitCommentsFor = commitCommentsFor' Nothing

-- | Just the comments on a specific SHA for a given Github repo.
-- With authentication.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor' :: Maybe Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error (Vector Comment))
commitCommentsFor' auth user repo sha =
    executeRequestMaybe auth $ commitCommentsForR user repo sha FetchAll

-- | List comments for a single commit.
-- See <https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit>
commitCommentsForR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request k (Vector Comment)
commitCommentsForR user repo sha =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "comments"] []

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor :: Name Owner -> Name Repo -> Id Comment -> IO (Either Error Comment)
commitCommentFor = commitCommentFor' Nothing

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor' :: Maybe Auth -> Name Owner -> Name Repo -> Id Comment -> IO (Either Error Comment)
commitCommentFor' auth user repo cid =
    executeRequestMaybe auth $ commitCommentForR user repo cid

-- | Query a single commit comment.
-- See <https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment>
commitCommentForR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
commitCommentForR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "comments", toPathPart cid] []
