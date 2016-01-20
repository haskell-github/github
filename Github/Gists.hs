-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The gists API as described at <http://developer.github.com/v3/gists/>.
module Github.Gists (
    gists,
    gists',
    gistsR,
    gist,
    gist',
    gistR,
    module Github.Data,
    ) where

import Data.Vector    (Vector)
import Github.Data
import Github.Request

-- | The list of all gists created by the user
--
-- > gists' (Just ("github-username", "github-password")) "mike-burns"
gists' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error (Vector Gist))
gists' auth user =
    executeRequestMaybe auth $ gistsR user Nothing

-- | The list of all public gists created by the user.
--
-- > gists "mike-burns"
gists :: Name GithubOwner -> IO (Either Error (Vector Gist))
gists = gists' Nothing

-- | List gists.
-- See <https://developer.github.com/v3/gists/#list-gists>
gistsR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector Gist)
gistsR user = GithubPagedGet ["users", toPathPart user, "gists"] []

-- | A specific gist, given its id, with authentication credentials
--
-- > gist' (Just ("github-username", "github-password")) "225074"
gist' :: Maybe GithubAuth -> Name Gist -> IO (Either Error Gist)
gist' auth gid =
    executeRequestMaybe auth $ gistR gid

-- | A specific gist, given its id.
--
-- > gist "225074"
gist :: Name Gist -> IO (Either Error Gist)
gist = gist' Nothing

-- | Get a single gist.
-- See <https://developer.github.com/v3/gists/#get-a-single-gist>
gistR :: Name Gist ->GithubRequest k Gist
gistR gid =
    GithubGet ["gists", toPathPart gid] []
