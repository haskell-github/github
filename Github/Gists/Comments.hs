-- | The loving comments people have left on Gists, described on
-- <http://developer.github.com/v3/gists/comments/>.
module Github.Gists.Comments (
    commentsOn,
    commentsOnR,
    comment,
    gistCommentR,
    module Github.Data,
    ) where

import Data.Vector (Vector)

import Github.Data
import Github.Request

-- | All the comments on a Gist, given the Gist ID.
--
-- > commentsOn "1174060"
commentsOn :: Name Gist -> IO (Either Error (Vector GistComment))
commentsOn gid =
    executeRequest' $ commentsOnR gid Nothing

-- | List comments on a gist.
-- See <https://developer.github.com/v3/gists/comments/#list-comments-on-a-gist>
commentsOnR :: Name Gist -> Maybe Count -> GithubRequest k (Vector GistComment)
commentsOnR gid =
    GithubPagedGet ["gists", toPathPart gid, "comments"] []

-- | A specific comment, by the comment ID.
--
-- > comment (Id 62449)
comment :: Id GistComment -> IO (Either Error GistComment)
comment cid =
    executeRequest' $ gistCommentR cid

-- | Get a single comment.
-- See <https://developer.github.com/v3/gists/comments/#get-a-single-comment>
gistCommentR :: Id GistComment -> GithubRequest k GistComment
gistCommentR cid =
    GithubGet ["gists", "comments", toPathPart cid] []
