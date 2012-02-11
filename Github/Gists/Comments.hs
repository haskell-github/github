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
-- > commentsOn def "1174060"
commentsOn :: GithubConfig -> String -> IO (Either Error [GistComment])
commentsOn c gistId = githubGet c ["gists", gistId, "comments"]

-- | A specific comment, by the comment ID.
--
-- > comment def "62449"
comment :: GithubConfig -> String -> IO (Either Error GistComment)
comment c commentId = githubGet c ["gists", "comments", commentId]
