module Github.Repos.Watching (
 watchersFor
,reposWatchedBy
,module Github.Data
) where

import Github.Data
import Github.Private

watchersFor :: String -> String -> IO (Either Error [GithubUser])
watchersFor userName repoName =
  githubGet ["repos", userName, repoName, "watchers"]

reposWatchedBy :: String -> IO (Either Error [Repo])
reposWatchedBy userName = githubGet ["users", userName, "watched"]
