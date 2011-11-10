module Github.GitData.Commits (
 commit
,module Github.Data
) where

import Github.Data
import Github.Private

commit :: String -> String -> String -> IO (Either Error GitCommit)
commit user repoName sha =
  githubGet ["repos", user, repoName, "git", "commits", sha]
