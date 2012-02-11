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
-- > forksFor def "thoughtbot" "paperclip"
forksFor :: GithubConfig -> String -> String -> IO (Either Error [Repo])
forksFor c userName repoName =
  githubGet c ["repos", userName, repoName, "forks"]
