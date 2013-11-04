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
-- > commitsFor "mike-burns" "github"
commitsFor :: String -> String -> IO (Either Error [Commit])
commitsFor user repo = githubGet ["repos", user, repo, "commits"]

-- | Details on a specific SHA1 for a repo.
--
-- > commit "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit :: String -> String -> String -> IO (Either Error Commit)
commit user repo sha1 = githubGet ["repos", user, repo, "commits", sha1]

-- | All the comments on a Github repo.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor :: String -> String -> IO (Either Error [Comment])
commentsFor user repo = githubGet ["repos", user, repo, "comments"]

-- | Just the comments on a specific SHA for a given Github repo.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor :: String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor user repo sha1 =
  githubGet ["repos", user, repo, "commits", sha1, "comments"]

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor :: String -> String -> String -> IO (Either Error Comment)
commitCommentFor user repo reqCommentId =
  githubGet ["repos", user, repo, "comments", reqCommentId]

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff :: String -> String -> String -> String -> IO (Either Error Diff)
diff user repo base headref =
  githubGet ["repos", user, repo, "compare", base ++ "..." ++ headref]
