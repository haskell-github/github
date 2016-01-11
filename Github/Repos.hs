{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module Github.Repos (
    -- * Querying repositories
    currentUserRepos,
    currentUserReposR,
    userRepos,
    userRepos',
    userReposR,
    organizationRepos,
    organizationRepos',
    organizationReposR,
    repository,
    repository',
    repositoryR,
    contributors,
    contributors',
    contributorsR,
    contributorsWithAnonymous,
    contributorsWithAnonymous',
    languagesFor,
    languagesFor',
    languagesForR,
    tagsFor,
    tagsFor',
    tagsForR,
    branchesFor,
    branchesFor',
    branchesForR,
    contentsFor,
    contentsFor',
    readmeFor,
    readmeFor',

    -- ** Create
    createRepo',
    createRepoR,
    createOrganizationRepo',
    createOrganizationRepoR,

    -- ** Edit
    editRepo,
    editRepoR,

    -- ** Delete
    deleteRepo,
    deleteRepoR,

    -- * Data
    module Github.Data,
    ) where

import Control.Applicative ((<|>))
import Data.Aeson.Compat   (encode)

import Data.Vector    (Vector)
import Github.Auth
import Github.Data
import Github.Request

import qualified Data.ByteString.Char8 as BS8

repoPublicityQueryString :: RepoPublicity -> QueryString
repoPublicityQueryString All     = [("type", Just "all")]
repoPublicityQueryString Owner   = [("type", Just "owner")]
repoPublicityQueryString Member  = [("type", Just "member")]
repoPublicityQueryString Public  = [("type", Just "public")]
repoPublicityQueryString Private = [("type", Just "private")]

-- | List your repositories.
currentUserRepos :: GithubAuth -> RepoPublicity -> IO (Either Error (Vector Repo))
currentUserRepos auth publicity =
    executeRequest auth $ currentUserReposR publicity Nothing

-- | List your repositories.
-- See <https://developer.github.com/v3/repos/#list-your-repositories>
currentUserReposR :: RepoPublicity -> Maybe Count -> GithubRequest k(Vector Repo)
currentUserReposR publicity =
    GithubPagedGet  ["user", "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | The repos for a user, by their login. Can be restricted to just repos they
-- own, are a member of, or publicize. Private repos will return empty list.
--
-- > userRepos "mike-burns" All
userRepos :: Name GithubOwner -> RepoPublicity -> IO (Either Error (Vector Repo))
userRepos = userRepos' Nothing

-- | The repos for a user, by their login.
-- With authentication.
--
-- > userRepos' (Just (GithubBasicAuth (user, password))) "mike-burns" All
userRepos' :: Maybe GithubAuth -> Name GithubOwner -> RepoPublicity -> IO (Either Error (Vector Repo))
userRepos' auth user publicity =
    executeRequestMaybe auth $ userReposR user publicity Nothing

-- | List user repositories.
-- See <https://developer.github.com/v3/repos/#list-user-repositories>
userReposR :: Name GithubOwner -> RepoPublicity -> Maybe Count -> GithubRequest k(Vector Repo)
userReposR user publicity =
    GithubPagedGet  ["users", toPathPart user, "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | The repos for an organization, by the organization name.
--
-- > organizationRepos "thoughtbot"
organizationRepos :: Name Organization -> IO (Either Error (Vector Repo))
organizationRepos org = organizationRepos' Nothing org All

-- | The repos for an organization, by the organization name.
-- With authentication.
--
-- > organizationRepos (Just (GithubBasicAuth (user, password))) "thoughtbot" All
organizationRepos' :: Maybe GithubAuth -> Name Organization -> RepoPublicity -> IO (Either Error (Vector Repo))
organizationRepos' auth org publicity =
    executeRequestMaybe auth $ organizationReposR org publicity Nothing

-- | List organization repositories.
-- See <https://developer.github.com/v3/repos/#list-organization-repositories>
organizationReposR :: Name Organization -> RepoPublicity -> Maybe Count -> GithubRequest k (Vector Repo)
organizationReposR org publicity =
    GithubPagedGet ["orgs", toPathPart org, "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | Details on a specific repo, given the owner and repo name.
--
-- > userRepo "mike-burns" "github"
repository :: Name GithubOwner -> Name Repo -> IO (Either Error Repo)
repository = repository' Nothing

-- | Details on a specific repo, given the owner and repo name.
-- With authentication.
--
-- > userRepo' (Just (GithubBasicAuth (user, password))) "mike-burns" "github"
repository' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error Repo)
repository' auth user repo =
    executeRequestMaybe auth $ repositoryR user repo

-- | Get single repository.
-- See <https://developer.github.com/v3/repos/#get>
repositoryR :: Name GithubOwner -> Name Repo -> GithubRequest k Repo
repositoryR user repo =
    GithubGet ["repos", toPathPart user, toPathPart repo] []

-- | Create a new repository.
--
-- > createRepo' (GithubBasicAuth (user, password)) (newRepo "some_repo") {newRepoHasIssues = Just False}
createRepo' :: GithubAuth -> NewRepo -> IO (Either Error Repo)
createRepo' auth nrepo =
    executeRequest auth $ createRepoR nrepo

-- | Create a new repository.
-- See <https://developer.github.com/v3/repos/#create>
createRepoR :: NewRepo -> GithubRequest 'True Repo
createRepoR nrepo =
    GithubPost Post ["user", "repos"] (encode nrepo)

-- | Create a new repository for an organization.
--
-- > createOrganizationRepo (GithubBasicAuth (user, password)) "thoughtbot" (newRepo "some_repo") {newRepoHasIssues = Just False}
createOrganizationRepo' :: GithubAuth -> Name Organization -> NewRepo -> IO (Either Error Repo)
createOrganizationRepo' auth org nrepo =
    executeRequest auth $ createOrganizationRepoR org nrepo

-- | Create a new repository for an organization.
-- See <https://developer.github.com/v3/repos/#create>
createOrganizationRepoR :: Name Organization -> NewRepo -> GithubRequest 'True Repo
createOrganizationRepoR org nrepo =
    GithubPost Post ["orgs", toPathPart org, "repos"] (encode nrepo)

-- | Edit an existing repository.
--
-- > editRepo (GithubBasicAuth (user, password)) "some_user" "some_repo" def {editDescription = Just "some description"}
editRepo :: GithubAuth
         -> Name GithubOwner      -- ^ owner
         -> Name Repo             -- ^ repository name
         -> EditRepo
         -> IO (Either Error Repo)
editRepo auth user repo body =
    executeRequest auth $ editRepoR user repo body


-- | Edit an existing repository.
-- See <https://developer.github.com/v3/repos/#edit>
editRepoR :: Name GithubOwner -> Name Repo -> EditRepo -> GithubRequest 'True Repo
editRepoR user repo body =
    GithubPost Patch ["repos", toPathPart user, toPathPart repo] (encode b)
  where
    -- if no name is given, use curent name
    b = body {editName = editName body <|> Just repo}

-- | The contributors to a repo, given the owner and repo name.
--
-- > contributors "thoughtbot" "paperclip"
contributors :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Contributor))
contributors = contributors' Nothing

-- | The contributors to a repo, given the owner and repo name.
-- With authentication.
--
-- > contributors' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
contributors' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Contributor))
contributors' auth user repo =
    executeRequestMaybe auth $ contributorsR user repo False Nothing

-- | List contributors.
-- See <https://developer.github.com/v3/repos/#list-contributors>
contributorsR :: Name GithubOwner
              -> Name Repo
              -> Bool              -- ^ Include anonymous
              -> Maybe Count
              -> GithubRequest k (Vector Contributor)
contributorsR user repo anon =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "contributors"] qs
  where
    qs | anon      = [("anon", Just "true")]
       | otherwise = []

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
--
-- > contributorsWithAnonymous "thoughtbot" "paperclip"
contributorsWithAnonymous :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Contributor))
contributorsWithAnonymous = contributorsWithAnonymous' Nothing

-- | The contributors to a repo, including anonymous contributors (such as
-- deleted users or git commits with unknown email addresses), given the owner
-- and repo name.
-- With authentication.
--
-- > contributorsWithAnonymous' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
contributorsWithAnonymous' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Contributor))
contributorsWithAnonymous' auth user repo =
    executeRequestMaybe auth $ contributorsR user repo True Nothing

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
--
-- > languagesFor "mike-burns" "ohlaunch"
languagesFor :: Name GithubOwner -> Name Repo -> IO (Either Error Languages)
languagesFor = languagesFor' Nothing

-- | The programming languages used in a repo along with the number of
-- characters written in that language. Takes the repo owner and name.
-- With authentication.
--
-- > languagesFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "ohlaunch"
languagesFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error Languages)
languagesFor' auth user repo =
    executeRequestMaybe auth $ languagesForR user repo

-- | List languages.
-- See <https://developer.github.com/v3/repos/#list-languages>
languagesForR :: Name GithubOwner -> Name Repo -> GithubRequest k Languages
languagesForR user repo =
    GithubGet  ["repos", toPathPart user, toPathPart repo, "languages"] []

-- | The git tags on a repo, given the repo owner and name.
--
-- > tagsFor "thoughtbot" "paperclip"
tagsFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Tag))
tagsFor = tagsFor' Nothing

-- | The git tags on a repo, given the repo owner and name.
-- With authentication.
--
-- > tagsFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
tagsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Tag))
tagsFor' auth user repo =
    executeRequestMaybe auth $ tagsForR user repo Nothing

-- | List tags.
-- See <https://developer.github.com/v3/repos/#list-tags>
tagsForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Tag)
tagsForR user repo =
    GithubPagedGet  ["repos", toPathPart user, toPathPart repo, "tags"] []

-- | The git branches on a repo, given the repo owner and name.
--
-- > branchesFor "thoughtbot" "paperclip"
branchesFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Branch))
branchesFor = branchesFor' Nothing

-- | The git branches on a repo, given the repo owner and name.
-- With authentication.
--
-- > branchesFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
branchesFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Branch))
branchesFor' auth user repo =
    executeRequestMaybe auth $ branchesForR user repo Nothing

-- | List branches.
-- See <https://developer.github.com/v3/repos/#list-branches>
branchesForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Branch)
branchesForR user repo =
    GithubPagedGet  ["repos", toPathPart user, toPathPart repo, "branches"] []

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
--
-- > contentsFor "thoughtbot" "paperclip" "README.md"
contentsFor :: Name GithubOwner -> Name Repo -> String -> Maybe String -> IO (Either Error Content)
contentsFor = contentsFor' Nothing

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
-- With Authentication
--
-- > contentsFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip" "README.md" Nothing
contentsFor' :: Maybe GithubAuth ->  Name GithubOwner -> Name Repo -> String -> Maybe String -> IO (Either Error Content)
contentsFor' auth user repo path ref =
    executeRequestMaybe auth $ contentsForR user repo path ref

contentsForR :: Name GithubOwner
             -> Name Repo
             -> String            -- ^ file or directory
             -> Maybe String      -- ^ Git commit
             -> GithubRequest k Content
contentsForR user repo path ref =
    GithubGet ["repos", toPathPart user, toPathPart repo, "contents", path] qs
  where
    qs =  maybe [] (\r -> [("ref", Just . BS8.pack $ r)]) ref

-- | The contents of a README file in a repo, given the repo owner and name
--
-- > readmeFor "thoughtbot" "paperclip"
readmeFor :: Name GithubOwner -> Name Repo -> IO (Either Error Content)
readmeFor = readmeFor' Nothing

-- | The contents of a README file in a repo, given the repo owner and name
-- With Authentication
--
-- > readmeFor' (Just (GithubBasicAuth (user, password))) "thoughtbot" "paperclip"
readmeFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error Content)
readmeFor' auth user repo =
    executeRequestMaybe auth $ readmeForR user repo

readmeForR :: Name GithubOwner -> Name Repo -> GithubRequest k Content
readmeForR user repo =
    GithubGet ["repos", toPathPart user, toPathPart repo, "readme"] []

-- | Delete an existing repository.
--
-- > deleteRepo (GithubBasicAuth (user, password)) "thoughtbot" "some_repo"
deleteRepo :: GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error ())
deleteRepo auth user repo =
    executeRequest auth $ deleteRepoR user repo

deleteRepoR :: Name GithubOwner -> Name Repo -> GithubRequest 'True ()
deleteRepoR user repo =
    GithubDelete ["repos", toPathPart user, toPathPart repo]
