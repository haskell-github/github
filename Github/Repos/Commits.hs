module Github.Repos.Commits (
 commitsFor
,commit
,commentsFor
,commitCommentsFor
,postCommentOn
,commitCommentFor
,updateCommentWith
,deleteComment
,module Github.Data
) where

import Github.Data
import Github.Repos.Commits.Private

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

postCommentOn :: String -> String -> String -> NewComment -> IO (Either Error Comment)
postCommentOn user repo sha1 newComment =
  githubPost ["repos", user, repo, "commits", sha1, "comments"]
             newComment

updateCommentWith :: String -> String -> String -> String -> IO (Either Error Comment)
updateCommentWith user repo commentId newBody =
  githubPatch ["repos", user, repo, "comments", commentId]
              UpdatedComment { updatedCommentBody = newBody }

-- skipped due to lack of Internet: compare two commits

deleteComment :: String -> String -> String -> IO (Either Error ())
deleteComment user repo commentId =
  githubDelete ["repos", user, repo, "comments", commentId]
