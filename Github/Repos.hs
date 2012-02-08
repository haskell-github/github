{-# LANGUAGE OverloadedStrings #-}
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
,BasicAuth
,def
,Edit(..)
,edit
) where

import Data.Default
import Data.Aeson.Types
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
-- > userRepos "mike-burns" All
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

-- | The repos for an organization, by the organization name.
--
-- > organizationRepos "thoughtbot"
organizationRepos :: String -> IO (Either Error [Repo])
organizationRepos orgName = githubGet ["orgs", orgName, "repos"]

-- | Details on a specific repo, given the owner and repo name.
--
-- > userRepo "mike-burns" "github"
userRepo :: String -> String -> IO (Either Error Repo)
userRepo userName repoName = githubGet ["repos", userName, repoName]

-- | The contributors to a repo, given the owner and repo name.
--
-- > contributors "thoughtbot" "paperclip"
contributors :: String -> String -> IO (Either Error [Contributor])
contributors userName repoName =
  githubGet ["repos", userName, repoName, "contributors"]

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
--
-- > contributorsWithAnonymous "thoughtbot" "paperclip"
contributorsWithAnonymous :: String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous userName repoName =
  githubGetWithQueryString
    ["repos", userName, repoName, "contributors"]
    "anon=true"

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
--
-- > languagesFor "mike-burns" "ohlaunch"
languagesFor :: String -> String -> IO (Either Error [Language])
languagesFor userName repoName = do
  result <- githubGet ["repos", userName, repoName, "languages"]
  return $ either Left (Right . getLanguages) result

-- | The git tags on a repo, given the repo owner and name.
--
-- > tagsFor "thoughtbot" "paperclip"
tagsFor :: String -> String -> IO (Either Error [Tag])
tagsFor userName repoName =
  githubGet ["repos", userName, repoName, "tags"]

-- | The git branches on a repo, given the repo owner and name.
--
-- > branchesFor "thoughtbot" "paperclip"
branchesFor :: String -> String -> IO (Either Error [Branch])
branchesFor userName repoName =
  githubGet ["repos", userName, repoName, "branches"]


data Edit = Edit {
  editName         :: Maybe String
, editDescription  :: Maybe String
, editHomepage     :: Maybe String
, editPublic       :: Maybe Bool
, editHasIssues    :: Maybe Bool
, editHasWiki      :: Maybe Bool
, editHasDownloads :: Maybe Bool
} deriving Show

instance Default Edit where
  def = Edit def def def def def def def

instance ToJSON  Edit where
  toJSON (Edit { editName         = name
               , editDescription  = description
               , editHomepage     = homepage
               , editPublic       = public
               , editHasIssues    = hasIssues
               , editHasWiki      = hasWiki
               , editHasDownloads = hasDownloads
               }) = object
               [ "name"          .= name
               , "description"   .= description
               , "homepage"      .= homepage
               , "public"        .= public
               , "has_issues"    .= hasIssues
               , "has_wiki"      .= hasWiki
               , "has_downloads" .= hasDownloads
               ]

-- | <http://developer.github.com/v3/repos/#edit>
--
-- Example:
--
-- > edit (user, password) "some_user" "some_repo" def {editDescription = "some description"}
edit :: BasicAuth
     -> String      -- ^ user/organization
     -> String      -- ^ repository name
     -> Edit
     -> IO (Either Error Repo)
edit auth user repo body = githubPatch auth ["repos", user, repo] body
