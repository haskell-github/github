{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo starring API as described on
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

import Data.Vector    (Vector)
import Github.Auth
import Github.Data
import Github.Request

-- | The list of users that have starred the specified Github repo.
--
-- > userInfoFor' Nothing "mike-burns"
stargazersFor :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimpleUser))
stargazersFor auth user repo =
    executeRequestMaybe auth $ stargazersForR user repo Nothing

-- | List Stargazers.
-- See <https://developer.github.com/v3/activity/starring/#list-stargazers>
stargazersForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector SimpleUser)
stargazersForR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "stargazers"] []

-- | All the public repos starred by the specified user.
--
-- > reposStarredBy Nothing "croaky"
reposStarredBy :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error (Vector Repo))
reposStarredBy auth user =
    executeRequestMaybe auth $ reposStarredByR user Nothing

-- | List repositories being starred.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
reposStarredByR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector Repo)
reposStarredByR user =
    GithubPagedGet ["users", toPathPart user, "starred"] []

-- | All the repos starred by the authenticated user.
myStarred :: GithubAuth -> IO (Either Error (Vector Repo))
myStarred auth =
    executeRequest auth $ myStarredR Nothing

-- | All the repos starred by the authenticated user.
myStarredR :: Maybe Count -> GithubRequest 'True (Vector Repo)
myStarredR = GithubPagedGet ["user", "starred"] []
