-- | The repo starring API as described on
-- <http://developer.github.com/v3/repos/starring/>.
module Github.Repos.Starring (
 stargazersFor
,reposStarredBy
,myStarred
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of users that have starred the specified Github repo.
--
-- > userInfoFor' Nothing "mike-burns"
stargazersFor :: Maybe GithubAuth -> String -> String -> IO (Either Error [GithubOwner])
stargazersFor auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "stargazers"]

-- | All the public repos starred by the specified user.
--
-- > reposStarredBy Nothing "croaky"
reposStarredBy :: Maybe GithubAuth -> String -> IO (Either Error [Repo])
reposStarredBy auth userName = githubGet' auth ["users", userName, "starred"]

-- | All the repos starred by the authenticated user.
myStarred :: GithubAuth -> IO (Either Error [Repo])
myStarred auth = githubGet' (Just auth) ["user", "starred"]
