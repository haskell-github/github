-- | Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module Github.Repos.Forks (
 forksFor
,forksFor'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: String -> String -> IO (Either Error [Repo])
forksFor = forksFor' Nothing

-- | All the repos that are forked off the given repo.
-- | With authentication
--
-- > forksFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
forksFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Repo])
forksFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "forks"]
