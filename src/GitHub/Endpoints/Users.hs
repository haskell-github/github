-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.
module GitHub.Endpoints.Users (
    userInfoFor,
    userInfoFor',
    userInfoForR,
    ownerInfoForR,
    userInfoCurrent',
    userInfoCurrentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | The information for a single user, by login name.
-- With authentification
--
-- > userInfoFor' (Just ("github-username", "github-password")) "mike-burns"
userInfoFor' :: Maybe Auth -> Name User -> IO (Either Error User)
userInfoFor' auth = executeRequestMaybe auth . userInfoForR

-- | The information for a single user, by login name.
--
-- > userInfoFor "mike-burns"
userInfoFor :: Name User -> IO (Either Error User)
userInfoFor = executeRequest' . userInfoForR

-- | Query a single user.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
userInfoForR :: Name User -> Request k User
userInfoForR user = query ["users", toPathPart user] []

-- | Query a single user or an organization.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
ownerInfoForR :: Name Owner -> Request k Owner
ownerInfoForR owner = query ["users", toPathPart owner] []

-- | Retrieve information about the user associated with the supplied authentication.
--
-- > userInfoCurrent' (OAuth "...")
userInfoCurrent' :: Auth -> IO (Either Error User)
userInfoCurrent' auth =
    executeRequest auth $ userInfoCurrentR

-- | Query the authenticated user.
-- See <https://developer.github.com/v3/users/#get-the-authenticated-user>
userInfoCurrentR :: Request 'RA User
userInfoCurrentR = query ["user"] []
