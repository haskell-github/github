module Github.Repos.Commits (
 commitsFor
,commit
,commentsFor
,commitCommentsFor
,postCommentOn
,module Github.Data
) where

import Github.Data
import Github.Repos.Commits.Private

commitsFor :: String -> String -> IO (Either Error [Commit])
commitsFor user repo = fullGithubGet ["repos", user, repo, "commits"]

commit :: String -> String -> String -> IO (Either Error Commit)
commit user repo sha1 = fullGithubGet ["repos", user, repo, "commits", sha1]

commentsFor :: String -> String -> IO (Either Error [Comment])
commentsFor user repo = fullGithubGet ["repos", user, repo, "comments"]

commitCommentsFor :: String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor user repo sha1 =
  fullGithubGet ["repos", user, repo, "commits", sha1, "comments"]

postCommentOn :: String -> String -> String -> NewComment -> IO (Either Error Comment)
postCommentOn user repo sha1 newComment =
  fullGithubPost ["repos", user, repo, "commits", sha1, "comments"]
                 newComment
