-- | The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module Github.GitData.References (
 reference
,references
,createReference
,namespacedReferences
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A single reference by the ref name.
--
-- > reference "mike-burns" "github" "heads/master"
reference :: String -> String -> String -> IO (Either Error GitReference)
reference user reqRepoName ref =
  githubGet ["repos", user, reqRepoName, "git", "refs", ref]

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references :: String -> String -> IO (Either Error [GitReference])
references user reqRepoName =
  githubGet ["repos", user, reqRepoName, "git", "refs"]

createReference :: GithubAuth -> String -> String -> NewGitReference -> IO (Either Error GitReference)
createReference auth owner reqRepoName newRef =
  githubPost auth ["repos", owner, reqRepoName, "git", "refs"] newRef

-- | Limited references by a namespace.
--
-- > namespacedReferences "thoughtbot" "paperclip" "tags"
namespacedReferences :: String -> String -> String -> IO (Either Error [GitReference])
namespacedReferences user reqRepoName namespace =
  githubGet ["repos", user, reqRepoName, "git", "refs", namespace]
