-- | Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module Github.Repos.Forks (
    forksFor,
    forksFor',
    forksForR,
    module Github.Data,
    ) where

import Github.Auth
import Github.Data
import Github.Request

-- | All the repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: Name GithubOwner -> Name Repo -> IO (Either Error [Repo])
forksFor = forksFor' Nothing

-- | All the repos that are forked off the given repo.
-- | With authentication
--
-- > forksFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
forksFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error [Repo])
forksFor' auth user repo =
    executeRequestMaybe auth $ forksForR user repo

-- | List forks.
-- See <https://developer.github.com/v3/repos/forks/#list-forks>
forksForR :: Name GithubOwner -> Name Repo -> GithubRequest k [Repo]
forksForR user repo =
    GithubGet ["repos", untagName user, untagName repo, "forks"] ""
