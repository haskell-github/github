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
    forkRepo,
    forkRepo',
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Data.Name
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
-- > forksFor' (Just $ BasicAuth "github-username" "github-password") "thoughtbot" "paperclip"
forksFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error  (Vector Repo))
forksFor' auth user repo =
    executeRequestMaybe auth $ forksForR user repo FetchAll

-- | List forks.
-- See <https://developer.github.com/v3/repos/forks/#list-forks>
forksForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Repo)
forksForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "forks"] []

forkRepo :: Auth -> Name Owner -> Name Repo -> Maybe (Name Organization) -> IO (Either Error Repo)
forkRepo auth user repo mOrg =
	executeRequest auth $ forkRepo' user repo mOrg

forkRepo' :: Name Owner -> Name Repo -> Maybe (Name Organization) -> Request 'RW Repo
forkRepo' user repo mOrg =
    let
      makeOrg :: Maybe (Name Organization) -> Value
      makeOrg Nothing = object []
      makeOrg (Just (N org)) = object ["organization" .= org ]
    in command Post ["repos", toPathPart user, toPathPart repo, "forks"] (encode $ makeOrg mOrg)
