{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
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
,contributors'
,contributorsWithAnonymous
,contributorsWithAnonymous'
,languagesFor
,languagesFor'
,tagsFor
,tagsFor'
,branchesFor
,branchesFor'
,contentsFor
,contentsFor'
,readmeFor
,readmeFor'
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

import Data.Data
import Data.Default
import Data.Aeson.Types
import Github.Data
import Github.Private
import GHC.Generics (Generic)
import Control.Applicative
import Control.DeepSeq (NFData)

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity =
    All     -- ^ All repos accessible to the user.
  | Owner   -- ^ Only repos owned by the user.
  | Public  -- ^ Only public repos.
  | Private -- ^ Only private repos.
  | Member  -- ^ Only repos to which the user is a member but not an owner.
 deriving (Show, Eq, Ord, Typeable, Data, Generic)

instance NFData RepoPublicity

repoPublicityQueryString :: RepoPublicity -> String
repoPublicityQueryString All     = "type=all"
repoPublicityQueryString Owner   = "type=owner"
repoPublicityQueryString Member  = "type=member"
repoPublicityQueryString Public  = "type=public"
repoPublicityQueryString Private = "type=private"

-- | The repos for a user, by their login. Can be restricted to just repos they
-- own, are a member of, or publicize. Private repos will return empty list.
--
-- > userRepos "mike-burns" All
userRepos :: String -> RepoPublicity -> IO (Either Error [Repo])
userRepos = userRepos' Nothing

-- | The repos for a user, by their login.
-- With authentication.
--
-- > userRepos' (Just (GithubBasicAuth (user, password))) "mike-burns" All
userRepos' :: Maybe GithubAuth -> String -> RepoPublicity -> IO (Either Error [Repo])
userRepos' auth userName publicity =
  githubGetWithQueryString' auth ["users", userName, "repos"] qs
  where qs = repoPublicityQueryString publicity

-- | The repos for an organization, by the organization name.
--
-- > organizationRepos "thoughtbot"
organizationRepos :: String -> IO (Either Error [Repo])
organizationRepos org = organizationRepos' Nothing org All

-- | The repos for an organization, by the organization name.
-- With authentication.
--
-- > organizationRepos (Just (GithubBasicAuth (user, password))) "thoughtbot" All
organizationRepos' :: Maybe GithubAuth -> String -> RepoPublicity -> IO (Either Error [Repo])
organizationRepos' auth orgName publicity =
  githubGetWithQueryString' auth ["orgs", orgName, "repos"] qs
  where qs = repoPublicityQueryString publicity


-- | A specific organization repo, by the organization name.
--
-- > organizationRepo "thoughtbot" "github"
organizationRepo :: String -> String -> IO (Either Error Repo)
organizationRepo = organizationRepo' Nothing

-- | A specific organization repo, by the organization name.
-- With authentication.
--
-- > organizationRepo (Just (GithubBasicAuth (user, password))) "thoughtbot" "github"
organizationRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error Repo)
organizationRepo' auth orgName reqRepoName = githubGet' auth ["orgs", orgName, reqRepoName]

-- | Details on a specific repo, given the owner and repo name.
--
-- > userRepo "mike-burns" "github"
userRepo :: String -> String -> IO (Either Error Repo)
userRepo = userRepo' Nothing

-- | Details on a specific repo, given the owner and repo name.
-- With authentication.
--
-- > userRepo' (Just (GithubBasicAuth (user, password))) "mike-burns" "github"
userRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error Repo)
userRepo' auth userName reqRepoName = githubGet' auth ["repos", userName, reqRepoName]

-- | The contributors to a repo, given the owner and repo name.
--
-- > contributors "thoughtbot" "paperclip"
contributors :: String -> String -> IO (Either Error [Contributor])
contributors = contributors' Nothing

-- | The contributors to a repo, given the owner and repo name.
-- With authentication.
--
-- > contributors' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
contributors' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Contributor])
contributors' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "contributors"]

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
--
-- > contributorsWithAnonymous "thoughtbot" "paperclip"
contributorsWithAnonymous :: String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous = contributorsWithAnonymous' Nothing

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
-- With authentication.
--
-- > contributorsWithAnonymous' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
contributorsWithAnonymous' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Contributor])
contributorsWithAnonymous' auth userName reqRepoName =
  githubGetWithQueryString' auth
    ["repos", userName, reqRepoName, "contributors"]
    "anon=true"


-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
--
-- > languagesFor "mike-burns" "ohlaunch"
languagesFor :: String -> String -> IO (Either Error [Language])
languagesFor = languagesFor' Nothing

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
-- With authentication.
--
-- > languagesFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "ohlaunch"
languagesFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Language])
languagesFor' auth userName reqRepoName = do
  result <- githubGet' auth ["repos", userName, reqRepoName, "languages"]
  return $ either Left (Right . getLanguages) result

-- | The git tags on a repo, given the repo owner and name.
--
-- > tagsFor "thoughtbot" "paperclip"
tagsFor :: String -> String -> IO (Either Error [Tag])
tagsFor = tagsFor' Nothing

-- | The git tags on a repo, given the repo owner and name.
-- With authentication.
--
-- > tagsFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
tagsFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Tag])
tagsFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "tags"]

-- | The git branches on a repo, given the repo owner and name.
--
-- > branchesFor "thoughtbot" "paperclip"
branchesFor :: String -> String -> IO (Either Error [Branch])
branchesFor = branchesFor' Nothing

-- | The git branches on a repo, given the repo owner and name.
-- With authentication.
--
-- > branchesFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
branchesFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Branch])
branchesFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "branches"]

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
--
-- > contentsFor "thoughtbot" "paperclip" "README.md"
contentsFor :: String -> String -> String -> Maybe String -> IO (Either Error Content)
contentsFor = contentsFor' Nothing

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
-- With Authentication
--
-- > contentsFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip" "README.md"
contentsFor' :: Maybe GithubAuth ->  String -> String -> String -> Maybe String -> IO (Either Error Content)
contentsFor' auth userName reqRepoName reqContentPath ref =
  githubGetWithQueryString' auth
  ["repos", userName, reqRepoName, "contents", reqContentPath] $
  maybe "" ("ref="++) ref

-- | The contents of a README file in a repo, given the repo owner and name
--
-- > readmeFor "thoughtbot" "paperclip"
readmeFor :: String -> String -> IO (Either Error Content)
readmeFor = readmeFor' Nothing

-- | The contents of a README file in a repo, given the repo owner and name
-- With Authentication
--
-- > readmeFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
readmeFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error Content)
readmeFor' auth userName reqRepoName =
  githubGetWithQueryString' auth
  ["repos", userName, reqRepoName, "readme"] $
  ""

data NewRepo = NewRepo {
  newRepoName         :: String
, newRepoDescription  :: (Maybe String)
, newRepoHomepage     :: (Maybe String)
, newRepoPrivate      :: (Maybe Bool)
, newRepoHasIssues    :: (Maybe Bool)
, newRepoHasWiki      :: (Maybe Bool)
, newRepoAutoInit     :: (Maybe Bool)
} deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData NewRepo

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
-- > createRepo (GithubBasicAuth (user, password)) (newRepo "some_repo") {newRepoHasIssues = Just False}
createRepo :: GithubAuth -> NewRepo -> IO (Either Error Repo)
createRepo auth = githubPost auth ["user", "repos"]

-- |
-- Create a new repository for an organization.
--
-- > createOrganizationRepo (GithubBasicAuth (user, password)) "thoughtbot" (newRepo "some_repo") {newRepoHasIssues = Just False}
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
} deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Edit

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
-- > editRepo (GithubBasicAuth (user, password)) "some_user" "some_repo" def {editDescription = Just "some description"}
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
-- > deleteRepo (GithubBasicAuth (user, password)) "thoughtbot" "some_repo"
deleteRepo :: GithubAuth
           -> String      -- ^ owner
           -> String      -- ^ repository name
           -> IO (Either Error ())
deleteRepo auth owner repo =
    githubAPIDelete auth $ buildPath ["repos", owner, repo]
