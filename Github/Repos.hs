-- | The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module Github.Repos (
 userRepos
,organizationRepos
,userRepo
,contributors
,contributorsWithAnonymous
,languagesFor
,tagsFor
,branchesFor
,module Github.Data
,RepoPublicity(..)
) where

import Github.Data
import Github.Private

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity =
    All     -- ^ All repos accessible to the user.
  | Owner   -- ^ Only repos owned by the user.
  | Public  -- ^ Only public repos.
  | Private -- ^ Only private repos.
  | Member  -- ^ Only repos to which the user is a member but not an owner.
 deriving (Show, Eq, Read)

-- | The repos for a user, by their login. Can be restricted to just repos they
-- own, are a member of, or publicize. Private repos are currently not
-- supported.
--
-- > userRepos def "mike-burns" All
userRepos :: GithubConfig -> String -> RepoPublicity -> IO (Either Error [Repo])
userRepos c userName All =
  githubGetWithQueryString c ["users", userName, "repos"] "type=all"
userRepos c userName Owner =
  githubGetWithQueryString c ["users", userName, "repos"] "type=owner"
userRepos c userName Member =
  githubGetWithQueryString c ["users", userName, "repos"] "type=member"
userRepos c userName Public =
  githubGetWithQueryString c ["users", userName, "repos"] "type=public"
userRepos c userName Private =
  return $ Left $ UserError "Cannot access private repos using userRepos"

-- | The repos for an organization, by the organization name.
--
-- > organizationRepos def "thoughtbot"
organizationRepos :: GithubConfig -> String -> IO (Either Error [Repo])
organizationRepos c orgName = githubGet c ["orgs", orgName, "repos"]

-- | Details on a specific repo, given the owner and repo name.
--
-- > userRepo def "mike-burns" "github"
userRepo :: GithubConfig -> String -> String -> IO (Either Error Repo)
userRepo c userName repoName = githubGet c ["repos", userName, repoName]

-- | The contributors to a repo, given the owner and repo name.
--
-- > contributors def "thoughtbot" "paperclip"
contributors :: GithubConfig -> String -> String -> IO (Either Error [Contributor])
contributors c userName repoName =
  githubGet c ["repos", userName, repoName, "contributors"]

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
--
-- > contributorsWithAnonymous def "thoughtbot" "paperclip"
contributorsWithAnonymous :: GithubConfig -> String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous c userName repoName =
  githubGetWithQueryString
    c
    ["repos", userName, repoName, "contributors"]
    "anon=true"

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
--
-- > languagesFor def "mike-burns" "ohlaunch"
languagesFor :: GithubConfig -> String -> String -> IO (Either Error [Language])
languagesFor c userName repoName = do
  result <- githubGet c ["repos", userName, repoName, "languages"]
  return $ either Left (Right . getLanguages) result

-- | The git tags on a repo, given the repo owner and name.
--
-- > tagsFor def "thoughtbot" "paperclip"
tagsFor :: GithubConfig -> String -> String -> IO (Either Error [Tag])
tagsFor c userName repoName =
  githubGet c ["repos", userName, repoName, "tags"]

-- | The git branches on a repo, given the repo owner and name.
--
-- > branchesFor def "thoughtbot" "paperclip"
branchesFor :: GithubConfig -> String -> String -> IO (Either Error [Branch])
branchesFor c userName repoName =
  githubGet c ["repos", userName, repoName, "branches"]
