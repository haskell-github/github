-- | The API for dealing with git blobs from Github repos, as described in
-- <http://developer.github.com/v3/git/blobs/>.
module Github.GitData.Blobs (
 blob
,blob'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | Get a blob by SHA1.
--
-- > blob' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Blob)
blob' auth user reqRepoName sha =
  githubGet' auth ["repos", user, reqRepoName, "git", "blobs", sha]


-- | Get a blob by SHA1.
--
-- > blob "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
blob :: String -> String -> String -> IO (Either Error Blob)
blob = blob' Nothing
