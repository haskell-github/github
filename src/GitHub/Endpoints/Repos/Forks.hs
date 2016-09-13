-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.
module GitHub.Endpoints.Repos.Forks (
    forksFor,
    forksFor',
    forksForR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the repos that are forked off the given repo.
--
-- > forksFor "thoughtbot" "paperclip"
forksFor :: Name Owner -> Name Repo -> IO (Either Error  (Vector Repo))
forksFor = forksFor' Nothing

-- | All the repos that are forked off the given repo.
-- | With authentication
--
-- > forksFor' (Just (User (user, password))) "thoughtbot" "paperclip"
forksFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error  (Vector Repo))
forksFor' auth user repo =
    executeRequestMaybe auth $ forksForR user repo FetchAll

-- | List forks.
-- See <https://developer.github.com/v3/repos/forks/#list-forks>
forksForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Repo)
forksForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "forks"] []
