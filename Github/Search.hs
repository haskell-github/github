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

import Github.Auth
import Github.Data
import Github.Request

-- | Perform a repository search.
-- With authentication.
--
-- > searchRepos' (Just $ GithubBasicAuth "github-username" "github-password') "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos' :: Maybe GithubAuth -> QueryString -> IO (Either Error SearchReposResult)
searchRepos' auth = executeRequestMaybe auth . searchReposR

-- | Perform a repository search.
-- Without authentication.
--
-- > searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos :: QueryString -> IO (Either Error SearchReposResult)
searchRepos = searchRepos' Nothing

-- | Search repositories.
--
-- See <https://developer.github.com/v3/search/#search-repositories>
searchReposR :: QueryString -> GithubRequest k SearchReposResult
searchReposR queryString = GithubGet ["search", "repositories"] queryString

-- | Perform a code search.
-- With authentication.
--
-- > searchCode' (Just $ GithubBasicAuth "github-username" "github-password') "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchCode' :: Maybe GithubAuth -> QueryString -> IO (Either Error SearchCodeResult)
searchCode' auth = executeRequestMaybe auth . searchCodeR

-- | Perform a code search.
-- Without authentication.
--
-- > searchCode "q=addClass+in:file+language:js+repo:jquery/jquery"
searchCode :: QueryString -> IO (Either Error SearchCodeResult)
searchCode = searchCode' Nothing

-- | Search code.
--
-- See <https://developer.github.com/v3/search/#search-code>
searchCodeR :: QueryString -> GithubRequest k SearchCodeResult
searchCodeR queryString = GithubGet ["search", "code"] queryString

-- | Perform an issue search.
-- With authentication.
--
-- > searchIssues' (Just $ GithubBasicAuth "github-username" "github-password') "q=a repo%3Aphadej%2Fgithub&per_page=100"
searchIssues' :: Maybe GithubAuth -> QueryString -> IO (Either Error SearchIssuesResult)
searchIssues' auth = executeRequestMaybe auth . searchIssuesR

-- | Perform an issue search.
-- Without authentication.
--
-- > searchIssues "q=a repo%3Aphadej%2Fgithub&per_page=100"
searchIssues :: QueryString -> IO (Either Error SearchIssuesResult)
searchIssues = searchIssues' Nothing

-- | Search issues.
--
-- See <https://developer.github.com/v3/search/#search-issues>
searchIssuesR :: QueryString -> GithubRequest k SearchIssuesResult
searchIssuesR queryString = GithubGet ["search", "issues"] queryString
