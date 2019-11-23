-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The user followers API as described on
-- <http://developer.github.com/v3/users/followers/>.
module GitHub.Endpoints.Users.Followers (
    usersFollowing,
    usersFollowedBy,
    usersFollowingR,
    usersFollowedByR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the users following the given user.
--
-- > usersFollowing "mike-burns"
usersFollowing :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowing user =
    executeRequest' $ usersFollowingR user FetchAll

-- | All the users following the given user, by their login.
-- With authentication.
--
-- > usersFollowing' (Just $ BasicAuth "github-username" "github-password") "mike-burns"
usersFollowing' :: Maybe Auth -> Name User -> IO (Either Error (Vector SimpleUser))
usersFollowing' auth user = executeRequestMaybe auth $ usersFollowingR user FetchAll


-- | List followers of a user.
-- See <https://developer.github.com/v3/users/followers/#list-followers-of-a-user>
usersFollowingR :: Name User -> FetchCount -> Request k (Vector SimpleUser)
usersFollowingR user =
    pagedQuery ["users", toPathPart user, "followers"] []

-- | All the users that the given user follows.
--
-- > usersFollowedBy "mike-burns"
usersFollowedBy :: Name User -> IO (Either Error (Vector SimpleUser))
usersFollowedBy user =
    executeRequest' $ usersFollowedByR user FetchAll

-- | All the users that the given user follows, by their login.
-- With authentication.
--
-- > usersFollowedBy' (Just $ BasicAuth "github-username" "github-password") "mike-burns"
usersFollowedBy' :: Maybe Auth -> Name User -> IO (Either Error (Vector SimpleUser))
usersFollowedBy' auth user = executeRequestMaybe auth $ usersFollowedByR user FetchAll

-- | List users followed by another user.
-- See <https://developer.github.com/v3/users/followers/#list-users-followed-by-another-user>
usersFollowedByR :: Name User -> FetchCount -> Request k (Vector SimpleUser)
usersFollowedByR user =
    pagedQuery ["users", toPathPart user, "following"] []
