-- | The repo commits API as described on
-- <http://developer.github.com/v3/repos/commits/>.
module Github.Repos.Commits (
 commitsFor
,commit
,commentsFor
,commitCommentsFor
,commitCommentFor
,diff
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The commit history for a repo.
--
-- > commitsFor def "mike-burns" "github"
commitsFor :: GithubConfig -> String -> String -> IO (Either Error [Commit])
commitsFor c user repo = githubGet c ["repos", user, repo, "commits"]

-- | Details on a specific SHA1 for a repo.
--
-- > commit def "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit :: GithubConfig -> String -> String -> String -> IO (Either Error Commit)
commit c user repo sha1 = githubGet c ["repos", user, repo, "commits", sha1]

-- | All the comments on a Github repo.
--
-- > commentsFor def "thoughtbot" "paperclip"
commentsFor :: GithubConfig -> String -> String -> IO (Either Error [Comment])
commentsFor c user repo = githubGet c ["repos", user, repo, "comments"]

-- | Just the comments on a specific SHA for a given Github repo.
--
-- > commitCommentsFor def "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor :: GithubConfig -> String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor c user repo sha1 =
  githubGet c ["repos", user, repo, "commits", sha1, "comments"]

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor def "thoughtbot" "paperclip" "669575"
commitCommentFor :: GithubConfig -> String -> String -> String -> IO (Either Error Comment)
commitCommentFor c user repo commentId =
  githubGet c ["repos", user, repo, "comments", commentId]

-- | The diff between two treeishes on a repo.
--
-- > diff def "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff :: GithubConfig -> String -> String -> String -> String -> IO (Either Error Diff)
diff c user repo base head =
  githubGet c ["repos", user, repo, "compare", base ++ "..." ++ head]
