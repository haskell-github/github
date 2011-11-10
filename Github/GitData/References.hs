module Github.GitData.References (
 reference
,references
,module Github.Data
) where

import Github.Data
import Github.Private

reference :: String -> String -> String -> IO (Either Error GitReference)
reference user repoName ref =
  githubGet ["repos", user, repoName, "git", "refs", ref]

references :: String -> String -> IO (Either Error [GitReference])
references user repoName =
  githubGet ["repos", user, repoName, "git", "refs"]
