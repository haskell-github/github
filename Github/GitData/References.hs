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
-- > reference def "mike-burns" "github" "heads/master"
reference :: GithubConfig -> String -> String -> String -> IO (Either Error GitReference)
reference c user repoName ref =
  githubGet c ["repos", user, repoName, "git", "refs", ref]

-- | The history of references for a repo.
--
-- > references def "mike-burns" "github"
references :: GithubConfig -> String -> String -> IO (Either Error [GitReference])
references c user repoName =
  githubGet c ["repos", user, repoName, "git", "refs"]

-- | Limited references by a namespace.
--
-- > namespacedReferences def "thoughtbot" "paperclip" "tags"
namespacedReferences :: GithubConfig -> String -> String -> String -> IO (Either Error [GitReference])
namespacedReferences c user repoName namespace =
  githubGet c ["repos", user, repoName, "git", "refs", namespace]
