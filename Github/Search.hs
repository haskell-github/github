{-# LANGUAGE OverloadedStrings #-}
-- | The Github Search API, as described at
-- <http://developer.github.com/v3/search/>.
module Github.Search(
    searchRepos',
    searchRepos,
    searchReposR,
    searchCode',
    searchCode,
    searchCodeR,
    searchIssues',
    searchIssues,
    searchIssuesR,
    module Github.Data,
    ) where

import Data.Text (Text)

import qualified Data.Text.Encoding as TE

import Github.Auth
import Github.Data
import Github.Request

-- | Perform a repository search.
-- With authentication.
--
-- > searchRepos' (Just $ GithubBasicAuth "github-username" "github-password') "a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos' :: Maybe GithubAuth -> Text -> IO (Either Error (SearchResult Repo))
searchRepos' auth = executeRequestMaybe auth . searchReposR

-- | Perform a repository search.
-- Without authentication.
--
-- > searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos :: Text -> IO (Either Error (SearchResult Repo))
searchRepos = searchRepos' Nothing

-- | Search repositories.
-- See <https://developer.github.com/v3/search/#search-repositories>
searchReposR :: Text -> GithubRequest k (SearchResult Repo)
searchReposR searchString = GithubGet ["search", "repositories"] [("q", Just $ TE.encodeUtf8 searchString)]

-- | Perform a code search.
-- With authentication.
--
-- > searchCode' (Just $ GithubBasicAuth "github-username" "github-password') "a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchCode' :: Maybe GithubAuth -> Text -> IO (Either Error (SearchResult Code))
searchCode' auth = executeRequestMaybe auth . searchCodeR

-- | Perform a code search.
-- Without authentication.
--
-- > searchCode "q=addClass+in:file+language:js+repo:jquery/jquery"
searchCode :: Text -> IO (Either Error (SearchResult Code))
searchCode = searchCode' Nothing

-- | Search code.
-- See <https://developer.github.com/v3/search/#search-code>
searchCodeR :: Text -> GithubRequest k (SearchResult Code)
searchCodeR searchString = GithubGet ["search", "code"] [("q", Just $ TE.encodeUtf8 searchString)]

-- | Perform an issue search.
-- With authentication.
--
-- > searchIssues' (Just $ GithubBasicAuth "github-username" "github-password') "a repo%3Aphadej%2Fgithub&per_page=100"
searchIssues' :: Maybe GithubAuth -> Text -> IO (Either Error (SearchResult Issue))
searchIssues' auth = executeRequestMaybe auth . searchIssuesR

-- | Perform an issue search.
-- Without authentication.
--
-- > searchIssues "q=a repo%3Aphadej%2Fgithub&per_page=100"
searchIssues :: Text -> IO (Either Error (SearchResult Issue))
searchIssues = searchIssues' Nothing

-- | Search issues.
-- See <https://developer.github.com/v3/search/#search-issues>
searchIssuesR :: Text -> GithubRequest k (SearchResult Issue)
searchIssuesR searchString = GithubGet ["search", "issues"] [("q", Just $ TE.encodeUtf8 searchString)]
