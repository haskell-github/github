-- | The Github Search API, as described at
-- <http://developer.github.com/v3/search/>.
module Github.Search(
 searchRepos'
,searchRepos
,searchCode'
,searchCode
,module Github.Data
) where

import Github.Data
import Github.Private

-- | Perform a repository search.
-- | With authentication.
--
-- > searchRepos' (Just $ GithubBasicAuth "github-username" "github-password') "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos' :: Maybe GithubAuth -> String -> IO (Either Error SearchReposResult)
searchRepos' auth queryString = githubGetWithQueryString' auth ["search", "repositories"] queryString

-- | Perform a repository search.
-- | Without authentication.
--
-- > searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos :: String -> IO (Either Error SearchReposResult)
searchRepos = searchRepos' Nothing 

-- | Perform a code search.
-- | With authentication.
--
-- > searchCode' (Just $ GithubBasicAuth "github-username" "github-password') "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchCode' :: Maybe GithubAuth -> String -> IO (Either Error SearchCodeResult)
searchCode' auth queryString = githubGetWithQueryString' auth ["search", "code"] queryString

-- | Perform a code search.
-- | Without authentication.
--
-- > searchCode "q=addClass+in:file+language:js+repo:jquery/jquery"
searchCode :: String -> IO (Either Error SearchCodeResult)
searchCode = searchCode' Nothing 


