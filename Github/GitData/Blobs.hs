module Github.GitData.Blobs (
 blob
,module Github.Data
) where

import Github.Data
import Github.Private

blob :: String -> String -> String -> IO (Either Error Blob)
blob user repoName sha =
  githubGet ["repos", user, repoName, "git", "blobs", sha]
