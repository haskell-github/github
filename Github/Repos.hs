module Github.Repos (
 userRepos
,module Github.Data
) where

import Github.Data
import Github.Private

userRepos :: String -> RepoPublicity -> IO (Either Error [Repo])
userRepos userName All =
  githubGetWithQueryString ["users", userName, "repos"] "type=all"
userRepos userName Owner =
  githubGetWithQueryString ["users", userName, "repos"] "type=owner"
userRepos userName Member =
  githubGetWithQueryString ["users", userName, "repos"] "type=member"
userRepos userName Public =
  githubGetWithQueryString ["users", userName, "repos"] "type=public"
userRepos userName Private =
  return $ Left $ UserError "Cannot access private repos using userRepos"
