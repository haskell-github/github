module Github.Repos.Forks (
 forksFor
,module Github.Data
) where

import Github.Data
import Github.Private

forksFor :: String -> String -> IO (Either Error [Repo])
forksFor userName repoName =
  githubGet ["repos", userName, repoName, "forks"]
