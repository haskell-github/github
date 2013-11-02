-- | The loving comments people have left on Gists, described on
-- <http://developer.github.com/v3/gists/comments/>.
module Github.Gists.Comments (
 commentsOn
,comment
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the comments on a Gist, given the Gist ID.
--
-- > commentsOn "1174060"
commentsOn :: String -> IO (Either Error [GistComment])
commentsOn reqGistId = githubGet ["gists", reqGistId, "comments"]

-- | A specific comment, by the comment ID.
--
-- > comment "62449"
comment :: String -> IO (Either Error GistComment)
comment reqCommentId = githubGet ["gists", "comments", reqCommentId]
