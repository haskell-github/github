module Github.Repos.Watching (
 watchersFor
,module Github.Data
) where

import Github.Data
import Github.Private

watchersFor :: String -> String -> IO (Either Error [GithubUser])
watchersFor userName repoName =
  githubGet ["repos", userName, repoName, "watchers"]
