module Github.Repos.Commits (
 commitsFor
,commit
,commentsFor
,module Github.Data
) where

import Github.Data
import Github.Repos.Commits.Private

commitsFor :: String -> String -> IO (Either String [Commit])
commitsFor user repo = do
  commitsJsonString <- githubApiGet $ buildUrl ["repos", user, repo, "commits"]
  return $ either Left parseCommitsJson commitsJsonString

commit :: String -> String -> String -> IO (Either String Commit)
commit user repo sha1 = do
  commitJsonString <- githubApiGet $ buildUrl ["repos", user, repo, "commits", sha1]
  return $ either Left parseCommitJson commitJsonString

commentsFor :: String -> String -> IO (Either String [Comment])
commentsFor user repo = do
  commentsJsonString <- githubApiGet $ buildUrl ["repos", user, repo, "comments"]
  return $ either Left parseCommentsJson commentsJsonString
