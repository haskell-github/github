-- | The loving comments people have left on Gists, described on
-- <http://developer.github.com/v3/gists/comments/>.
module Github.Gists.Comments (
    commentsOn,
    commentsOnR,
    comment,
    gistCommentR,
    module Github.Data,
    ) where

import Github.Data
import Github.Request

-- | All the comments on a Gist, given the Gist ID.
--
-- > commentsOn "1174060"
commentsOn :: Name Gist -> IO (Either Error [GistComment])
commentsOn gid =
    executeRequest' $ commentsOnR gid

-- | List comments on a gist.
-- See <https://developer.github.com/v3/gists/comments/#list-comments-on-a-gist>
commentsOnR :: Name Gist -> GithubRequest k [GistComment]
commentsOnR gid =
    GithubGet ["gists", untagName gid, "comments"] ""

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
    GithubGet ["gists", "comments", show $ untagId cid] ""
