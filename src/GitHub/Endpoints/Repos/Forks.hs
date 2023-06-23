-- |
-- Hot forking action, as described at
-- <http://developer.github.com/v3/repos/forks/>.

module GitHub.Endpoints.Repos.Forks (
    forksForR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List forks.
-- See <https://developer.github.com/v3/repos/forks/#list-forks>
forksForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Repo)
forksForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "forks"] []
