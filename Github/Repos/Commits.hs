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

commitsFor :: String -> String -> IO (Either Error [Commit])
commitsFor user repo = githubGet ["repos", user, repo, "commits"]

commit :: String -> String -> String -> IO (Either Error Commit)
commit user repo sha1 = githubGet ["repos", user, repo, "commits", sha1]

commentsFor :: String -> String -> IO (Either Error [Comment])
commentsFor user repo = githubGet ["repos", user, repo, "comments"]

commitCommentsFor :: String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor user repo sha1 =
  githubGet ["repos", user, repo, "commits", sha1, "comments"]

commitCommentFor :: String -> String -> String -> IO (Either Error Comment)
commitCommentFor user repo commentId =
  githubGet ["repos", user, repo, "comments", commentId]

diff :: String -> String -> String -> String -> IO (Either Error Diff)
diff user repo base head =
  githubGet ["repos", user, repo, "compare", base ++ "..." ++ head]
