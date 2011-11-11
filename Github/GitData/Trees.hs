module Github.GitData.Trees (
 tree
,nestedTree
,module Github.Data
) where

import Github.Data
import Github.Private

tree :: String -> String -> String -> IO (Either Error Tree)
tree user repoName sha =
  githubGet ["repos", user, repoName, "git", "trees", sha]

nestedTree :: String -> String -> String -> IO (Either Error Tree)
nestedTree user repoName sha =
  githubGetWithQueryString ["repos", user, repoName, "git", "trees", sha]
                           "recursive=1"
