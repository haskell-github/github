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
    starGist,
    starGistR,
    unstarGist,
    unstarGistR,
    deleteGist,
    deleteGistR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

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
gistsR user = pagedQuery ["users", toPathPart user, "gists"] []

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
gistR :: Name Gist -> Request k Gist
gistR gid =
    query ["gists", toPathPart gid] []

-- | Star a gist by the authenticated user.
--
-- > starGist ("github-username", "github-password") "225074"
starGist :: Auth -> Name Gist -> IO (Either Error ())
starGist auth gid = executeRequest auth $ starGistR gid

-- | Star a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#star-a-gist>
starGistR :: Name Gist -> GenRequest 'MtUnit 'RW ()
starGistR gid = Command Put ["gists", toPathPart gid, "star"] mempty

-- | Unstar a gist by the authenticated user.
--
-- > unstarGist ("github-username", "github-password") "225074"
unstarGist :: Auth -> Name Gist -> IO (Either Error ())
unstarGist auth gid = executeRequest auth $ unstarGistR gid

-- | Unstar a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#unstar-a-gist>
unstarGistR :: Name Gist -> Request 'RW ()
unstarGistR gid = command Delete ["gists", toPathPart gid, "star"] mempty

-- | Delete a gist by the authenticated user.
--
-- > deleteGist ("github-username", "github-password") "225074"
deleteGist :: Auth -> Name Gist -> IO (Either Error ())
deleteGist auth gid = executeRequest auth $ deleteGistR gid

-- | Delete a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#delete-a-gist>
deleteGistR :: Name Gist -> Request 'RW ()
deleteGistR gid = command Delete ["gists", toPathPart gid] mempty
