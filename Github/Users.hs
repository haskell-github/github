{-# LANGUAGE DataKinds #-}
-- | The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.
module Github.Users (
 userInfoFor
,userInfoFor'
,userInfoForR
,userInfoCurrent'
,userInfoCurrentR
,module Github.Data
) where

import Github.Data
import Github.Request

-- | The information for a single user, by login name.
-- With authentification
--
-- > userInfoFor' (Just ("github-username", "github-password")) "mike-burns"
userInfoFor' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error GithubOwner)
userInfoFor' auth = executeRequestMaybe auth . userInfoForR

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: Name GithubOwner -> IO (Either Error GithubOwner)
userInfoFor = executeRequest' . userInfoForR

-- | Get a single user.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
userInfoForR :: Name GithubOwner -> GithubRequest k GithubOwner
userInfoForR userName = GithubGet ["users", toPathPart userName] []

-- | Retrieve information about the user associated with the supplied authentication.
--
-- > userInfoCurrent' (GithubOAuth "...")
userInfoCurrent' :: GithubAuth -> IO (Either Error GithubOwner)
userInfoCurrent' auth =
    executeRequest auth $ userInfoCurrentR

-- | Get the authenticated user.
-- See <https://developer.github.com/v3/users/#get-the-authenticated-user>
userInfoCurrentR :: GithubRequest 'True GithubOwner
userInfoCurrentR = GithubGet ["user"] []
