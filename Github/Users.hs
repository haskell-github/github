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
userInfoFor' :: Maybe GithubAuth -> String -> IO (Either Error DetailedOwner)
userInfoFor' auth = executeRequestMaybe auth . userInfoForR

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: String -> IO (Either Error DetailedOwner)
userInfoFor = executeRequest' .  userInfoForR

-- | The information for a single user, by login name. The request
userInfoForR :: String -> GithubRequest k DetailedOwner
userInfoForR userName = GithubGet ["users", userName] ""

-- | Retrieve information about the user associated with the supplied authentication.
--
-- > userInfoCurrent' (GithubOAuth "...")
--
-- TODO: Change to require 'GithubAuth'?
userInfoCurrent' :: Maybe GithubAuth -> IO (Either Error DetailedOwner)
userInfoCurrent' auth =
    executeRequestMaybe auth . unsafeDropAuthRequirements $ userInfoCurrentR

-- | Retrieve information about the user associated with the supplied authentication.
userInfoCurrentR :: GithubRequest 'True DetailedOwner
userInfoCurrentR = GithubGet ["user"] ""
