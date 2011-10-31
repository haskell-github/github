module Github.Repos.Commits (
 commitsFor
,commit
,commentsFor
,module Github.Data
) where

import Github.Data
import Github.Repos.Commits.Private

commitsFor :: String -> String -> IO (Either String [Commit])
commitsFor user repo = fullGithubGet ["repos", user, repo, "commits"]

commit :: String -> String -> String -> IO (Either String Commit)
commit user repo sha1 = fullGithubGet ["repos", user, repo, "commits", sha1]

commentsFor :: String -> String -> IO (Either String [Comment])
commentsFor user repo = fullGithubGet ["repos", user, repo, "comments"]
