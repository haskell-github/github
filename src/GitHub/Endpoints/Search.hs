-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Search API, as described at
-- <http://developer.github.com/v3/search/>.
module GitHub.Endpoints.Search(
    searchRepos',
    searchRepos,
    searchReposR,
    searchCode',
    searchCode,
    searchCodeR,
    searchIssues',
    searchIssues,
    searchIssuesR,
    W.EscapeItem(..),
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import qualified Network.HTTP.Types as W
import Prelude ()

-- | Perform a repository search.
-- With authentication (5000 queries per hour).
--
-- >     let query search = search { searchRepoOptionsLanguage = Just (Language "Haskell")
--                                 , searchRepoOptionsSortBy   = Just Stars
--                                 , searchRepoOptionsOrder    = Just SortDescending
--                                 , searchRepoOptionsCreated  = Just (start, newDate)
--                                 }
--    res <- searchRepos' (Just $ BasicAuth "github-username" "github-password") (SearchRepoMod query)
searchRepos' :: Maybe Auth -> SearchRepoMod -> IO (Either Error (SearchResult Repo))
searchRepos' auth opts = executeRequestMaybe auth $ searchReposR opts

-- | Perform a repository search.
-- Without authentication (60 queries per hour).
--
-- >     let query search = search { searchRepoOptionsLanguage = Just (Language "Haskell")
--                                 , searchRepoOptionsSortBy   = Just Stars
--                                 , searchRepoOptionsOrder    = Just SortDescending
--                                 , searchRepoOptionsCreated  = Just (start, newDate)
--                                 }
--    res <- searchRepos (SearchRepoMod query)
searchRepos :: SearchRepoMod -> IO (Either Error (SearchResult Repo))
searchRepos = searchRepos' Nothing

-- | Search repositories.
-- See <https://developer.github.com/v3/search/#search-repositories>
searchReposR :: SearchRepoMod -> Request k (SearchResult Repo)
searchReposR opts =
    query ["search", "repositories"] qs
  where
    qs = searchRepoModToQueryString opts

-- | Perform a code search.
-- With authentication (5000 queries per hour).
--
-- QE = URI encode
-- QN = Not URI encode
-- >    res <- searchCode' (Just $ BasicAuth "github-username" "github-password")
--                       [("q", [QE "language", QN ":", QE "haskell"]),
--                        ("sort", [QE "stars"]),
--                        ("order", [QE "desc"])]
searchCode' :: Maybe Auth -> QueryString -> IO (Either Error (SearchResult Code))
searchCode' auth = executeRequestMaybe auth . searchCodeR

-- | Perform a code search.
-- Without authentication (60 queries per hour).
--
-- >    res <- searchCode'  [("q", [QE "language", QN ":", QE "haskell"]),
--                         ("sort", [QE "stars"]),
--                         ("order", [QE "desc"])]
searchCode :: QueryString -> IO (Either Error (SearchResult Code))
searchCode = searchCode' Nothing

-- | Search code.
-- See <https://developer.github.com/v3/search/#search-code>
searchCodeR :: QueryString -> Request k (SearchResult Code)
searchCodeR searchString =
    query ["search", "code"] searchString

-- | Perform an issue search.
-- With authentication.
--
-- Because of URI encoding 
-- "q=a+repo:phadej/github&per_page=100"
-- has to be written as
-- > searchIssues' (Just $ BasicAuth "github-username" "github-password")
--                   [("q", [QE "a", QN "+", QE "repo", QN ":", QE "phadej", QN "/", QE "github"]),
--                    ("per_page", [QE "100"])]
searchIssues' :: Maybe Auth -> QueryString -> IO (Either Error (SearchResult Issue))
searchIssues' auth = executeRequestMaybe auth . searchIssuesR

-- | Perform an issue search.
-- Without authentication.
--
-- "q=a+repo:phadej/github&per_page=100"
-- has to be written as
-- > searchIssues [("q", [QE "a", QN "+", QE "repo", QN ":", QE "phadej", QN "/", QE "github"]),
--                 ("per_page", [QE "100"])]
searchIssues :: QueryString -> IO (Either Error (SearchResult Issue))
searchIssues = searchIssues' Nothing

-- | Search issues.
-- See <https://developer.github.com/v3/search/#search-issues>
searchIssuesR :: QueryString -> Request k (SearchResult Issue)
searchIssuesR searchString =
    query ["search", "issues"] searchString
