{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module Github.Repos (

-- * Querying repositories
 userRepos
,userRepos'
,userRepo
,userRepo'
,organizationRepos
,organizationRepos'
,organizationRepo
,organizationRepo'
,contributors
,contributorsWithAnonymous
,languagesFor
,tagsFor
,branchesFor
,module Github.Data
,RepoPublicity(..)

-- ** Create
,createRepo
,createOrganizationRepo
,newRepo
,NewRepo(..)

-- ** Edit
,editRepo
,def
,Edit(..)

-- ** Delete
,deleteRepo
) where

import Data.Default
import Data.Aeson.Types
import Github.Data
import Github.Private
import Network.HTTP.Conduit
import Control.Applicative
import qualified Control.Exception as E
import Network.HTTP.Types

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity =
    All     -- ^ All repos accessible to the user.
  | Owner   -- ^ Only repos owned by the user.
  | Public  -- ^ Only public repos.
  | Private -- ^ Only private repos.
  | Member  -- ^ Only repos to which the user is a member but not an owner.
 deriving (Show, Eq)

-- | The repos for a user, by their login. Can be restricted to just repos they
-- own, are a member of, or publicize. Private repos are currently not
-- supported.
--
-- > userRepos "mike-burns" All
userRepos :: String -> RepoPublicity -> IO (Either Error [Repo])
userRepos = userRepos' Nothing

-- | The repos for a user, by their login.
-- | With authentication, but note that private repos are currently not supported.
--
-- > userRepos' (Just (GithubUser (user, password))) "mike-burns" All
userRepos' :: Maybe GithubAuth -> String -> RepoPublicity -> IO (Either Error [Repo])
userRepos' auth userName All =
  githubGetWithQueryString' auth ["users", userName, "repos"] "type=all"
userRepos' auth userName Owner =
  githubGetWithQueryString' auth ["users", userName, "repos"] "type=owner"
userRepos' auth userName Member =
  githubGetWithQueryString' auth ["users", userName, "repos"] "type=member"
userRepos' auth userName Public =
  githubGetWithQueryString' auth ["users", userName, "repos"] "type=public"
userRepos' _auth _userName Private =
  return $ Left $ UserError "Cannot access private repos using userRepos"

-- | The repos for an organization, by the organization name.
--
-- > organizationRepos "thoughtbot"
organizationRepos :: String -> IO (Either Error [Repo])
organizationRepos = organizationRepos' Nothing

-- | The repos for an organization, by the organization name.
-- | With authentication
--
-- > organizationRepos (Just (GithubUser (user, password))) "thoughtbot"
organizationRepos' :: Maybe GithubAuth -> String -> IO (Either Error [Repo])
organizationRepos' auth orgName = githubGet' auth ["orgs", orgName, "repos"]

-- | A specific organization repo, by the organization name.
--
-- > organizationRepo "thoughtbot" "github"
organizationRepo :: String -> String -> IO (Either Error Repo)
organizationRepo = organizationRepo' Nothing

-- | A specific organization repo, by the organization name.
-- | With authentication
--
-- > organizationRepo (Just (GithubUser (user, password))) "thoughtbot" "github"
organizationRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error Repo)
organizationRepo' auth orgName reqRepoName = githubGet' auth ["orgs", orgName, reqRepoName]

-- | Details on a specific repo, given the owner and repo name.
--
-- > userRepo "mike-burns" "github"
userRepo :: String -> String -> IO (Either Error Repo)
userRepo = userRepo' Nothing

-- | Details on a specific repo, given the owner and repo name.
-- | With authentication
--
-- > userRepo' (Just (GithubUser (user, password))) "mike-burns" "github"
userRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error Repo)
userRepo' auth userName reqRepoName = githubGet' auth ["repos", userName, reqRepoName]

-- | The contributors to a repo, given the owner and repo name.
--
-- > contributors "thoughtbot" "paperclip"
contributors :: String -> String -> IO (Either Error [Contributor])
contributors userName reqRepoName =
  githubGet ["repos", userName, reqRepoName, "contributors"]

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
--
-- > contributorsWithAnonymous "thoughtbot" "paperclip"
contributorsWithAnonymous :: String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous userName reqRepoName =
  githubGetWithQueryString
    ["repos", userName, reqRepoName, "contributors"]
    "anon=true"

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
--
-- > languagesFor "mike-burns" "ohlaunch"
languagesFor :: String -> String -> IO (Either Error [Language])
languagesFor userName reqRepoName = do
  result <- githubGet ["repos", userName, reqRepoName, "languages"]
  return $ either Left (Right . getLanguages) result

-- | The git tags on a repo, given the repo owner and name.
--
-- > tagsFor "thoughtbot" "paperclip"
tagsFor :: String -> String -> IO (Either Error [Tag])
tagsFor userName reqRepoName =
  githubGet ["repos", userName, reqRepoName, "tags"]

-- | The git branches on a repo, given the repo owner and name.
--
-- > branchesFor "thoughtbot" "paperclip"
branchesFor :: String -> String -> IO (Either Error [Branch])
branchesFor userName reqRepoName =
  githubGet ["repos", userName, reqRepoName, "branches"]


data NewRepo = NewRepo {
  newRepoName         :: String
, newRepoDescription  :: (Maybe String)
, newRepoHomepage     :: (Maybe String)
, newRepoPrivate      :: (Maybe Bool)
, newRepoHasIssues    :: (Maybe Bool)
, newRepoHasWiki      :: (Maybe Bool)
, newRepoAutoInit     :: (Maybe Bool)
} deriving Show

instance ToJSON  NewRepo where
  toJSON (NewRepo { newRepoName         = name
                  , newRepoDescription  = description
                  , newRepoHomepage     = homepage
                  , newRepoPrivate      = private
                  , newRepoHasIssues    = hasIssues
                  , newRepoHasWiki      = hasWiki
                  , newRepoAutoInit     = autoInit
                  }) = object
                  [ "name"                .= name
                  , "description"         .= description
                  , "homepage"            .= homepage
                  , "private"             .= private
                  , "has_issues"          .= hasIssues
                  , "has_wiki"            .= hasWiki
                  , "auto_init"           .= autoInit
                  ]

newRepo :: String -> NewRepo
newRepo name = NewRepo name Nothing Nothing Nothing Nothing Nothing Nothing

-- |
-- Create a new repository.
--
-- > createRepo (GithubUser (user, password)) (newRepo "some_repo") {newRepoHasIssues = Just False}
createRepo :: GithubAuth -> NewRepo -> IO (Either Error Repo)
createRepo auth = githubPost auth ["user", "repos"]

-- |
-- Create a new repository for an organization.
--
-- > createOrganizationRepo (GithubUser (user, password)) "thoughtbot" (newRepo "some_repo") {newRepoHasIssues = Just False}
createOrganizationRepo :: GithubAuth -> String -> NewRepo -> IO (Either Error Repo)
createOrganizationRepo auth org = githubPost auth ["orgs", org, "repos"]

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

-- |
-- Edit an existing repository.
--
-- > editRepo (GithubUser (user, password)) "some_user" "some_repo" def {editDescription = Just "some description"}
editRepo :: GithubAuth
     -> String      -- ^ owner
     -> String      -- ^ repository name
     -> Edit
     -> IO (Either Error Repo)
editRepo auth user repo body = githubPatch auth ["repos", user, repo] b
  where
    -- if no name is given, use curent name
    b = body {editName = editName body <|> Just repo}

-- |
-- Delete an existing repository.
--
-- > deleteRepo (GithubUser (user, password)) "thoughtbot" "some_repo"
deleteRepo :: GithubAuth
           -> String      -- ^ owner
           -> String      -- ^ repository name
           -> IO (Either Error ())
deleteRepo auth owner repo = do
  result <- doHttps "DELETE" url (Just auth) Nothing
  case result of
      Left e -> return (Left (HTTPConnectionError e))
      Right resp ->
          let status = responseStatus resp
              headers = responseHeaders resp
          in if status == notFound404
                -- doHttps silently absorbs 404 errors, but for this operation
                -- we want the user to know if they've tried to delete a
                -- non-existent repository
             then return (Left (HTTPConnectionError
                                (E.toException
                                 (StatusCodeException status headers
#if MIN_VERSION_http_conduit(1, 9, 0)
                                 (responseCookieJar resp)
#endif
                                 ))))
             else return (Right ())
  where
    url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo
