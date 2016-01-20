{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.
module Github.Users (
    userInfoFor,
    userInfoFor',
    userInfoForR,
    ownerInfoForR,
    userInfoCurrent',
    userInfoCurrentR,
    module Github.Data,
    ) where

import Github.Data
import Github.Request

-- | The information for a single user, by login name.
-- With authentification
--
-- > userInfoFor' (Just ("github-username", "github-password")) "mike-burns"
userInfoFor' :: Maybe GithubAuth -> Name User -> IO (Either Error User)
userInfoFor' auth = executeRequestMaybe auth . userInfoForR

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: Name User -> IO (Either Error User)
userInfoFor = executeRequest' . userInfoForR

-- | Get a single user.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
userInfoForR :: Name User -> GithubRequest k User
userInfoForR user = GithubGet ["users", toPathPart user] []

-- | Get a single user or an organization.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
ownerInfoForR :: Name GithubOwner -> GithubRequest k GithubOwner
ownerInfoForR owner = GithubGet ["users", toPathPart owner] []

-- | Retrieve information about the user associated with the supplied authentication.
--
-- > userInfoCurrent' (GithubOAuth "...")
userInfoCurrent' :: GithubAuth -> IO (Either Error User)
userInfoCurrent' auth =
    executeRequest auth $ userInfoCurrentR

-- | Get the authenticated user.
-- See <https://developer.github.com/v3/users/#get-the-authenticated-user>
userInfoCurrentR :: GithubRequest 'True User
userInfoCurrentR = GithubGet ["user"] []
