-- |
-- The Github Users API, as described at
-- <http://developer.github.com/v3/users/>.

module GitHub.Endpoints.Users (
    userInfoForR,
    ownerInfoForR,
    userInfoCurrentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import Prelude ()

-- | Query a single user.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
--
-- >>> github' userInfoForR "mike-burns"
--
-- or
--
-- >>> github userInfoForR (OAuth "github-token") "mike-burns"
--
userInfoForR :: Name User -> Request k User
userInfoForR user = query ["users", toPathPart user] []

-- | Query a single user or an organization.
-- See <https://developer.github.com/v3/users/#get-a-single-user>
ownerInfoForR :: Name Owner -> Request k Owner
ownerInfoForR owner = query ["users", toPathPart owner] []

-- | Query the authenticated user.
-- See <https://developer.github.com/v3/users/#get-the-authenticated-user>
userInfoCurrentR :: Request 'RA User
userInfoCurrentR = query ["user"] []
