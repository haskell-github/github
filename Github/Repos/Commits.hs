module Github.Repos.Commits (
 commitsFor
,module Github.Data
) where

import Github.Data
import Github.Repos.Commits.Private

commitsFor :: String -> String -> IO (Either String [Commit])
commitsFor user repo = do
  commitsJsonString <- githubApiGet $ buildUrl ["repos", user, repo, "commits"]
  return $ either Left parseCommitsJson commitsJsonString
