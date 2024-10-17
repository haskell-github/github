-- |
-- The repo commits API as described on
-- <http://developer.github.com/v3/repos/comments/>.

module GitHub.Endpoints.Repos.Comments (
    commentsForR,
    commitCommentsForR,
    commitCommentForR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List commit comments for a repository.
-- See <https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository>
commentsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Comment)
commentsForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "comments"] []

-- | List comments for a single commit.
-- See <https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit>
commitCommentsForR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request k (Vector Comment)
commitCommentsForR user repo sha =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "comments"] []

-- | Query a single commit comment.
-- See <https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment>
commitCommentForR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
commitCommentForR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "comments", toPathPart cid] []
