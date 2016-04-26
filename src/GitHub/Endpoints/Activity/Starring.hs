{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings   #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo starring API as described on
-- <https://developer.github.com/v3/activity/starring/>.
module GitHub.Endpoints.Activity.Starring (
    stargazersFor,
    stargazersForR,
    reposStarredBy,
    reposStarredByR,
    myStarred,
    myStarredR,
    myStarredAcceptStar,
    myStarredAcceptStarR,
    module GitHub.Data,
    ) where

import Data.Vector    (Vector)
import GitHub.Auth
import GitHub.Data
import GitHub.Request

-- | The list of users that have starred the specified Github repo.
--
-- > userInfoFor' Nothing "mike-burns"
stargazersFor :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector SimpleUser))
stargazersFor auth user repo =
    executeRequestMaybe auth $ stargazersForR user repo Nothing

-- | List Stargazers.
-- See <https://developer.github.com/v3/activity/starring/#list-stargazers>
stargazersForR :: Name Owner -> Name Repo -> Maybe Count -> Request k (Vector SimpleUser)
stargazersForR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "stargazers"] []

-- | All the public repos starred by the specified user.
--
-- > reposStarredBy Nothing "croaky"
reposStarredBy :: Maybe Auth -> Name Owner -> IO (Either Error (Vector Repo))
reposStarredBy auth user =
    executeRequestMaybe auth $ reposStarredByR user Nothing

-- | List repositories being starred.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
reposStarredByR :: Name Owner -> Maybe Count -> Request k (Vector Repo)
reposStarredByR user =
    PagedQuery ["users", toPathPart user, "starred"] []

-- | All the repos starred by the authenticated user.
myStarred :: Auth -> IO (Either Error (Vector Repo))
myStarred auth =
    executeRequest auth $ myStarredR Nothing

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
myStarredR :: Maybe Count -> Request 'True (Vector Repo)
myStarredR = PagedQuery ["user", "starred"] []


-- | All the repos starred by the authenticated user.
myStarredAcceptStar :: Auth -> IO (Either Error (Vector RepoStarred))
myStarredAcceptStar auth =
    executeRequest auth $ myStarredAcceptStarR Nothing

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1>
myStarredAcceptStarR :: Maybe Count -> Request 'True (Vector RepoStarred)
myStarredAcceptStarR = HeaderQuery [("Accept", "application/vnd.github.v3.star+json")] . PagedQuery ["user", "starred"] []
