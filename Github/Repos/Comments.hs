{-# LANGUAGE CPP #-}

-- | The repo commits API as described on
-- <http://developer.github.com/v3/repos/comments/>.
module Github.Repos.Comments (
    commentsFor,
    commentsFor',
    commentsForR,
    commitCommentsFor,
    commitCommentsFor',
    commitCommentsForR,
    commitCommentFor,
    commitCommentFor',
    commitCommentForR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | All the comments on a Github repo.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Comment))
commentsFor = commentsFor' Nothing

-- | All the comments on a Github repo.
-- With authentication.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Comment))
commentsFor' auth user repo =
    executeRequestMaybe auth $ commentsForR user repo Nothing

-- | List commit comments for a repository.
-- See <https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository>
commentsForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Comment)
commentsForR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "comments"] []

-- | Just the comments on a specific SHA for a given Github repo.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor :: Name GithubOwner -> Name Repo -> Name Commit -> IO (Either Error (Vector Comment))
commitCommentsFor = commitCommentsFor' Nothing

-- | Just the comments on a specific SHA for a given Github repo.
-- With authentication.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Commit -> IO (Either Error (Vector Comment))
commitCommentsFor' auth user repo sha =
    executeRequestMaybe auth $ commitCommentsForR user repo sha Nothing

-- | List comments for a single commit.
-- See <https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit>
commitCommentsForR :: Name GithubOwner -> Name Repo -> Name Commit -> Maybe Count -> GithubRequest k (Vector Comment)
commitCommentsForR user repo sha =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "comments"] []

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor :: Name GithubOwner -> Name Repo -> Id Comment -> IO (Either Error Comment)
commitCommentFor = commitCommentFor' Nothing

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Comment -> IO (Either Error Comment)
commitCommentFor' auth user repo cid =
    executeRequestMaybe auth $ commitCommentForR user repo cid

-- | Get a single commit comment.
-- See <https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment>
commitCommentForR :: Name GithubOwner -> Name Repo -> Id Comment -> GithubRequest k Comment
commitCommentForR user repo cid =
    GithubGet ["repos", toPathPart user, toPathPart repo, "comments", toPathPart cid] []
