-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The gists API as described at <http://developer.github.com/v3/gists/>.
module GitHub.Endpoints.Gists (
    gists,
    gists',
    gistsR,
    gist,
    gist',
    gistR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Request
import GitHub.Internal.Prelude

-- | The list of all gists created by the user
--
-- > gists' (Just ("github-username", "github-password")) "mike-burns"
gists' :: Maybe Auth -> Name Owner -> IO (Either Error (Vector Gist))
gists' auth user =
    executeRequestMaybe auth $ gistsR user FetchAll

-- | The list of all public gists created by the user.
--
-- > gists "mike-burns"
gists :: Name Owner -> IO (Either Error (Vector Gist))
gists = gists' Nothing

-- | List gists.
-- See <https://developer.github.com/v3/gists/#list-gists>
gistsR :: Name Owner -> FetchCount -> Request k (Vector Gist)
gistsR user = PagedQuery ["users", toPathPart user, "gists"] []

-- | A specific gist, given its id, with authentication credentials
--
-- > gist' (Just ("github-username", "github-password")) "225074"
gist' :: Maybe Auth -> Name Gist -> IO (Either Error Gist)
gist' auth gid =
    executeRequestMaybe auth $ gistR gid

-- | A specific gist, given its id.
--
-- > gist "225074"
gist :: Name Gist -> IO (Either Error Gist)
gist = gist' Nothing

-- | Query a single gist.
-- See <https://developer.github.com/v3/gists/#get-a-single-gist>
gistR :: Name Gist ->Request k Gist
gistR gid =
    Query ["gists", toPathPart gid] []
