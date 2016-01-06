{-# LANGUAGE DataKinds #-}
-- | The repo starring API as described on
-- <https://developer.github.com/v3/activity/starring/>.
module Github.Activity.Starring (
    stargazersFor,
    stargazersForR,
    reposStarredBy,
    reposStarredByR,
    myStarred,
    myStarredR,
    module Github.Data,
    ) where

import Github.Auth
import Github.Data
import Github.Request

-- | The list of users that have starred the specified Github repo.
--
-- > userInfoFor' Nothing "mike-burns"
stargazersFor :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error [GithubOwner])
stargazersFor auth user repo =
    executeRequestMaybe auth $ stargazersForR user repo

-- | List Stargazers.
-- See <https://developer.github.com/v3/activity/starring/#list-stargazers>
stargazersForR :: Name GithubOwner -> Name Repo -> GithubRequest k [GithubOwner]
stargazersForR user repo =
    GithubGet ["repos", untagName user, untagName repo, "stargazers"] ""

-- | All the public repos starred by the specified user.
--
-- > reposStarredBy Nothing "croaky"
reposStarredBy :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error [Repo])
reposStarredBy auth user =
    executeRequestMaybe auth $ reposStarredByR user

-- | List repositories being starred.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
reposStarredByR :: Name GithubOwner -> GithubRequest k [Repo]
reposStarredByR user =
    GithubGet ["users", untagName user, "starred"] ""

-- | All the repos starred by the authenticated user.
myStarred :: GithubAuth -> IO (Either Error [Repo])
myStarred auth =
    executeRequest auth $ myStarredR

-- | All the repos starred by the authenticated user.
myStarredR :: GithubRequest 'True [Repo]
myStarredR = GithubGet ["user", "starred"] ""
