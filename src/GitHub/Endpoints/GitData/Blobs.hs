-- |
-- The API for dealing with git blobs from Github repos, as described in
-- <http://developer.github.com/v3/git/blobs/>.

module GitHub.Endpoints.GitData.Blobs (
    blobR,
    module GitHub.Data,
    ) where

import GitHub.Data
import Prelude ()

-- | Query a blob.
-- See <https://developer.github.com/v3/git/blobs/#get-a-blob>
blobR :: Name Owner -> Name Repo -> Name Blob -> Request k Blob
blobR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "git", "blobs", toPathPart sha] []
