-- | The repo watching API as described on
-- <http://developer.github.com/v3/repos/watching/>.
module Github.Repos.Watching (
 watchersFor
,watchersFor'
,reposWatchedBy
,reposWatchedBy'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of users that are watching the specified Github repo.
--
-- > watchersFor "thoughtbot" "paperclip"
watchersFor :: String -> String -> IO (Either Error [GithubOwner])
watchersFor = watchersFor' Nothing

-- | The list of users that are watching the specified Github repo.
-- | With authentication
--
-- > watchersFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
watchersFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [GithubOwner])
watchersFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "watchers"]

-- | All the public repos watched by the specified user.
--
-- > reposWatchedBy "croaky"
reposWatchedBy :: String -> IO (Either Error [Repo])
reposWatchedBy = reposWatchedBy' Nothing

-- | All the public repos watched by the specified user.
-- | With authentication
--
-- > reposWatchedBy' (Just (GithubUser (user, password))) "croaky"
reposWatchedBy' :: Maybe GithubAuth -> String -> IO (Either Error [Repo])
reposWatchedBy' auth userName = githubGet' auth ["users", userName, "watched"]
