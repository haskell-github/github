-- | The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module Github.GitData.References (
 reference
,references
,namespacedReferences
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A single reference by the ref name.
--
-- > reference "mike-burns" "github" "heads/master"
reference :: String -> String -> String -> IO (Either Error GitReference)
reference user repoName ref =
  githubGet ["repos", user, repoName, "git", "refs", ref]

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references :: String -> String -> IO (Either Error [GitReference])
references user repoName =
  githubGet ["repos", user, repoName, "git", "refs"]

-- | Limited references by a namespace.
--
-- > namespacedReferences "thoughtbot" "paperclip" "tags"
namespacedReferences :: String -> String -> String -> IO (Either Error [GitReference])
namespacedReferences user repoName namespace =
  githubGet ["repos", user, repoName, "git", "refs", namespace]
