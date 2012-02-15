-- | Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module Github.Repos.Forks (
 forksFor
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: String -> String -> IO (Either Error [Repo])
forksFor userName repoName =
  githubGet ["repos", userName, repoName, "forks"]
