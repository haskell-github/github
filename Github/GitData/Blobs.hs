-- | The API for dealing with git blobs from Github repos, as described in
-- <http://developer.github.com/v3/git/blobs/>.
module Github.GitData.Blobs (
    blob,
    blob',
    blobR,
    module Github.Data,
    ) where

import Github.Data
import Github.Request

-- | Get a blob by SHA1.
--
-- > blob' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Blob -> IO (Either Error Blob)
blob' auth user repo sha =
    executeRequestMaybe auth $ blobR user repo sha

-- | Get a blob by SHA1.
--
-- > blob "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob :: Name GithubOwner -> Name Repo -> Name Blob -> IO (Either Error Blob)
blob = blob' Nothing

-- | Get a blob.
-- See <https://developer.github.com/v3/git/blobs/#get-a-blob>
blobR :: Name GithubOwner -> Name Repo -> Name Blob -> GithubRequest k Blob
blobR user repo sha =
    GithubGet ["repos", toPathPart user, toPathPart repo, "git", "blobs", toPathPart sha] []
