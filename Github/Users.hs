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

import Github.Auth
import Github.Data
import Github.Request

-- | The information for a single user, by login name.
-- With authentification
--
-- > userInfoFor' (Just ("github-username", "github-password")) "mike-burns"
userInfoFor' :: Maybe GithubAuth -> Name DetailedOwner -> IO (Either Error DetailedOwner)
userInfoFor' auth = executeRequestMaybe auth . userInfoForR

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: Name DetailedOwner -> IO (Either Error DetailedOwner)
userInfoFor = executeRequest' . userInfoForR

-- | Get a single user.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
userInfoForR :: Name DetailedOwner -> GithubRequest k DetailedOwner
userInfoForR userName = GithubGet ["users", untagName userName] []

-- | Retrieve information about the user associated with the supplied authentication.
--
-- > userInfoCurrent' (GithubOAuth "...")
--
-- TODO: Change to require 'GithubAuth'?
userInfoCurrent' :: Maybe GithubAuth -> IO (Either Error DetailedOwner)
userInfoCurrent' auth =
    executeRequestMaybe auth . unsafeDropAuthRequirements $ userInfoCurrentR

-- | Get the authenticated user.
-- See <https://developer.github.com/v3/users/#get-the-authenticated-user>
userInfoCurrentR :: GithubRequest 'True DetailedOwner
userInfoCurrentR = GithubGet ["user"] []
