-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Search API, as described at
-- <http://developer.github.com/v3/search/>.
module GitHub.Endpoints.Search(
    searchReposR,
    searchCodeR,
    searchIssuesR,
    searchUsersR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text.Encoding as TE

-- | Search repositories.
-- See <https://developer.github.com/v3/search/#search-repositories>
searchReposR :: Text -> Request k (SearchResult Repo)
searchReposR searchString =
    query ["search", "repositories"] [("q", Just $ TE.encodeUtf8 searchString)]

-- | Search code.
-- See <https://developer.github.com/v3/search/#search-code>
searchCodeR :: Text -> Request k (SearchResult Code)
searchCodeR searchString =
    query ["search", "code"] [("q", Just $ TE.encodeUtf8 searchString)]

-- | Search issues.
-- See <https://developer.github.com/v3/search/#search-issues>
searchIssuesR :: Text -> Request k (SearchResult Issue)
searchIssuesR searchString =
    query ["search", "issues"] [("q", Just $ TE.encodeUtf8 searchString)]

-- | Search users.
-- See <https://developer.github.com/v3/search/#search-code>
searchUsersR :: Text -> Request k (SearchResult SimpleUser)
searchUsersR searchString =
  query ["search", "users"] [("q", Just $ TE.encodeUtf8 searchString)]
