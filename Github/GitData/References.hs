-- | The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module Github.GitData.References (
 reference
,reference'
,references
,references'
,createReference
,namespacedReferences
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A single reference by the ref name.
--
-- > reference' (Just ("github-username", "github-password")) "mike-burns" "github" "heads/master"
reference' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error GitReference)
reference' auth user reqRepoName ref =
    githubGet' auth ["repos", user, reqRepoName, "git", "refs", ref]

-- | A single reference by the ref name.
--
-- > reference "mike-burns" "github" "heads/master"
reference :: String -> String -> String -> IO (Either Error GitReference)
reference = reference' Nothing

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references' :: Maybe GithubAuth -> String -> String -> IO (Either Error [GitReference])
references' auth user reqRepoName =
    githubGet' auth ["repos", user, reqRepoName, "git", "refs"]

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references :: String -> String -> IO (Either Error [GitReference])
references = references' Nothing
  

createReference :: GithubAuth -> String -> String -> NewGitReference -> IO (Either Error GitReference)
createReference auth owner reqRepoName newRef =
  githubPost auth ["repos", owner, reqRepoName, "git", "refs"] newRef

-- | Limited references by a namespace.
--
-- > namespacedReferences "thoughtbot" "paperclip" "tags"
namespacedReferences :: String -> String -> String -> IO (Either Error [GitReference])
namespacedReferences user reqRepoName namespace =
  githubGet ["repos", user, reqRepoName, "git", "refs", namespace]
