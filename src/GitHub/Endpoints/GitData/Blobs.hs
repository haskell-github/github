-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The API for dealing with git blobs from Github repos, as described in
-- <http://developer.github.com/v3/git/blobs/>.
module GitHub.Endpoints.GitData.Blobs (
    blob,
    blob',
    blobR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | Query a blob by SHA1.
--
-- > blob' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob' :: Maybe Auth -> Name Owner -> Name Repo -> Name Blob -> IO (Either Error Blob)
blob' auth user repo sha =
    executeRequestMaybe auth $ blobR user repo sha

-- | Query a blob by SHA1.
--
-- > blob "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob :: Name Owner -> Name Repo -> Name Blob -> IO (Either Error Blob)
blob = blob' Nothing

-- | Query a blob.
-- See <https://developer.github.com/v3/git/blobs/#get-a-blob>
blobR :: Name Owner -> Name Repo -> Name Blob -> Request k Blob
blobR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "git", "blobs", toPathPart sha] []
