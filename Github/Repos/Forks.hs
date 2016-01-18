-- | Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module Github.Repos.Forks (
    forksFor,
    forksFor',
    forksForR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | All the repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: Name GithubOwner -> Name Repo -> IO (Either Error  (Vector Repo))
forksFor = forksFor' Nothing

-- | All the repos that are forked off the given repo.
-- | With authentication
--
-- > forksFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
forksFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error  (Vector Repo))
forksFor' auth user repo =
    executeRequestMaybe auth $ forksForR user repo Nothing

-- | List forks.
-- See <https://developer.github.com/v3/repos/forks/#list-forks>
forksForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Repo)
forksForR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "forks"] []
