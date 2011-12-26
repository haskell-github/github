module Github.Repos (
 userRepos
,organizationRepos
,userRepo
,contributors
,contributorsWithAnonymous
,languagesFor
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

organizationRepos :: String -> IO (Either Error [Repo])
organizationRepos orgName = githubGet ["orgs", orgName, "repos"]

userRepo :: String -> String -> IO (Either Error Repo)
userRepo userName repoName = githubGet ["repos", userName, repoName]

contributors :: String -> String -> IO (Either Error [Contributor])
contributors userName repoName =
  githubGet ["repos", userName, repoName, "contributors"]

contributorsWithAnonymous :: String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous userName repoName =
  githubGetWithQueryString
    ["repos", userName, repoName, "contributors"]
    "anon=true"

languagesFor :: String -> String -> IO (Either Error [Language])
languagesFor userName repoName = do
  result <- githubGet ["repos", userName, repoName, "languages"]
  return $ either Left (Right . getLanguages) result
