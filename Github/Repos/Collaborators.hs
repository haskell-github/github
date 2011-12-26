module Github.Repos.Collaborators (
collaboratorsOn
,module Github.Data
) where

import Github.Data
import Github.Private

collaboratorsOn :: String -> String -> IO (Either Error [GithubUser])
collaboratorsOn userName repoName =
  githubGet ["repos", userName, repoName, "collaborators"]
