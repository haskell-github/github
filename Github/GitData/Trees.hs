module Github.GitData.Trees (
 tree
,module Github.Data
) where

import Github.Data
import Github.Private

tree :: String -> String -> String -> IO (Either Error Tree)
tree user repoName sha =
  githubGet ["repos", user, repoName, "git", "trees", sha]
