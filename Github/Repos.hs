{-# LANGUAGE OverloadedStrings #-}
-- | The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module Github.Repos (

-- * Querying repositories
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

-- * Modifying repositories
-- |
-- Only authenticated users may modify repositories.  Currently only
-- /HTTP basic access authentication/ is implemented.
,BasicAuth

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
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
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


data NewRepo = NewRepo {
  newRepoName         :: String
, newRepoDescription  :: (Maybe String)
, newRepoHomepage     :: (Maybe String)
, newRepoPrivate      :: (Maybe Bool)
, newRepoHasIssues    :: (Maybe Bool)
, newRepoHasWiki      :: (Maybe Bool)
, newRepoHasDownloads :: (Maybe Bool)
} deriving Show

instance ToJSON  NewRepo where
  toJSON (NewRepo { newRepoName         = name
                  , newRepoDescription  = description
                  , newRepoHomepage     = homepage
                  , newRepoPrivate      = private
                  , newRepoHasIssues    = hasIssues
                  , newRepoHasWiki      = hasWiki
                  , newRepoHasDownloads = hasDownloads
                  }) = object
                  [ "name"                .= name
                  , "description"         .= description
                  , "homepage"            .= homepage
                  , "private"             .= private
                  , "has_issues"          .= hasIssues
                  , "has_wiki"            .= hasWiki
                  , "has_downloads"       .= hasDownloads
                  ]

newRepo :: String -> NewRepo
newRepo name = NewRepo name Nothing Nothing Nothing Nothing Nothing Nothing

-- |
-- Create a new repository.
--
-- > createRepo (user, password) (newRepo "some_repo") {newRepoHasIssues = Just False}
createRepo :: BasicAuth -> NewRepo -> IO (Either Error Repo)
createRepo auth = githubPost auth ["user", "repos"]

-- |
-- Create a new repository for an organization.
--
-- > createOrganizationRepo (user, password) "thoughtbot" (newRepo "some_repo") {newRepoHasIssues = Just False}
createOrganizationRepo :: BasicAuth -> String -> NewRepo -> IO (Either Error Repo)
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
-- > editRepo (user, password) "some_user" "some_repo" def {editDescription = Just "some description"}
editRepo :: BasicAuth
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
-- > deleteRepo (user, password) "thoughtbot" "some_repo"
deleteRepo :: BasicAuth
           -> String      -- ^ owner
           -> String      -- ^ repository name
           -> IO (Either Error ())
deleteRepo auth owner repo = do
  requestToken >>= either (return . Left) (sendToken)
  where
    -- APIv3 does not support deletion, so we use APIv2 for that
    url = "https://github.com/api/v2/json/repos/delete/" ++ owner ++ "/" ++ repo

    requestToken :: IO (Either Error DeleteToken)
    requestToken = githubAPI "POST" url (Just auth) (Nothing :: Maybe Value)

    sendToken (DeleteToken t) = do
      let body = RequestBodyBS $ renderSimpleQuery False [("delete_token", t)]
      result <- doHttps "POST" url (Just auth) (Just body)
      return $ either (Left . HTTPConnectionError)
                      (const $ Right ())
                      result

newtype DeleteToken = DeleteToken BS.ByteString
  deriving Show

instance FromJSON DeleteToken where
  parseJSON (Object o) = DeleteToken <$> o .: "delete_token"
  parseJSON _          = fail "Could not build a DeleteToken"
