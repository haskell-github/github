module Github.GitData.References (
 reference
,module Github.Data
) where

import Github.Data
import Github.Private

reference :: String -> String -> String -> IO (Either Error GitReference)
reference user repoName ref =
  githubGet ["repos", user, repoName, "git", "refs", ref]

