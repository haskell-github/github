-- | Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module Github.Repos.Forks (
 forksFor
,forksForPage
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The first page of repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: String -> String -> IO (Either Error [Repo])
forksFor username reponame =
  forksForPage username reponame 1

-- | A given page of repos that are forked off the given repo.
--
-- > forksForPage "thoughtbot" "paperclip" 1
forksForPage :: String -> String -> Int -> IO (Either Error [Repo])
forksForPage userName repoName =
  githubGetPage ["repos", userName, repoName, "forks"]
