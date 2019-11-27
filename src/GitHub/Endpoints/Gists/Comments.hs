-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The loving comments people have left on Gists, described on
-- <http://developer.github.com/v3/gists/comments/>.
module GitHub.Endpoints.Gists.Comments (
    commentsOnR,
    gistCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List comments on a gist.
-- See <https://developer.github.com/v3/gists/comments/#list-comments-on-a-gist>
commentsOnR :: Name Gist -> FetchCount -> Request k (Vector GistComment)
commentsOnR gid =
    pagedQuery ["gists", toPathPart gid, "comments"] []

-- | Query a single comment.
-- See <https://developer.github.com/v3/gists/comments/#get-a-single-comment>
gistCommentR :: Id GistComment -> Request k GistComment
gistCommentR cid =
    query ["gists", "comments", toPathPart cid] []
