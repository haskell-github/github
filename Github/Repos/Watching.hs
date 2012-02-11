-- | The repo watching API as described on
-- <http://developer.github.com/v3/repos/watching/>.
module Github.Repos.Watching (
 watchersFor
,reposWatchedBy
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of users that are watching the specified Github repo.
--
-- > watchersFor def "thoughtbot" "paperclip"
watchersFor :: GithubConfig -> String -> String -> IO (Either Error [GithubOwner])
watchersFor c userName repoName =
  githubGet c ["repos", userName, repoName, "watchers"]

-- | All the public repos watched by the specified user.
--
-- > reposWatchedBy def "croaky"
reposWatchedBy :: GithubConfig -> String -> IO (Either Error [Repo])
reposWatchedBy c userName = githubGet c ["users", userName, "watched"]
