-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The gists API as described at <http://developer.github.com/v3/gists/>.
module GitHub.Endpoints.Gists (
    gistsR,
    gistR,
    createGistR,
    starGistR,
    unstarGistR,
    deleteGistR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List gists.
-- See <https://developer.github.com/v3/gists/#list-gists>
gistsR :: Name Owner -> FetchCount -> Request k (Vector Gist)
gistsR user = pagedQuery ["users", toPathPart user, "gists"] []

-- | Query a single gist.
-- See <https://developer.github.com/v3/gists/#get-a-single-gist>
gistR :: Name Gist -> Request k Gist
gistR gid =
    query ["gists", toPathPart gid] []

-- | Create a new gist
-- See <https://docs.github.com/rest/reference/gists#create-a-gist>
createGistR :: NewGist -> Request 'RW Gist
createGistR ngist = command Post ["gists"] (encode ngist)

-- | Star a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#star-a-gist>
starGistR :: Name Gist -> GenRequest 'MtUnit 'RW ()
starGistR gid = Command Put ["gists", toPathPart gid, "star"] mempty

-- | Unstar a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#unstar-a-gist>
unstarGistR :: Name Gist -> GenRequest 'MtUnit 'RW ()
unstarGistR gid = Command Delete ["gists", toPathPart gid, "star"] mempty

-- | Delete a gist by the authenticated user.
-- See <https://developer.github.com/v3/gists/#delete-a-gist>
deleteGistR :: Name Gist -> GenRequest 'MtUnit 'RW ()
deleteGistR gid = Command Delete ["gists", toPathPart gid] mempty
