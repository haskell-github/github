-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module GitHub.Endpoints.GitData.References (
    reference,
    reference',
    referenceR,
    references,
    references',
    referencesR,
    createReference,
    createReferenceR,
    namespacedReferences,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | A single reference by the ref name.
--
-- > reference' (Just ("github-username", "github-password")) "mike-burns" "github" "heads/master"
reference' :: Maybe Auth -> Name Owner -> Name Repo -> Name GitReference -> IO (Either Error GitReference)
reference' auth user repo ref =
    executeRequestMaybe auth $ referenceR user repo ref

-- | A single reference by the ref name.
--
-- > reference "mike-burns" "github" "heads/master"
reference :: Name Owner -> Name Repo -> Name GitReference -> IO (Either Error GitReference)
reference = reference' Nothing

-- | Query a reference.
-- See <https://developer.github.com/v3/git/refs/#get-a-reference>
referenceR :: Name Owner -> Name Repo -> Name GitReference -> Request k GitReference
referenceR user repo ref =
    query ["repos", toPathPart user, toPathPart repo, "git", "refs", toPathPart ref] []

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector GitReference))
references' auth user repo =
    executeRequestMaybe auth $ referencesR user repo FetchAll

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references :: Name Owner -> Name Repo -> IO (Either Error (Vector GitReference))
references = references' Nothing

-- | Query all References.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
referencesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector GitReference)
referencesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "git", "refs"] []

-- | Create a reference.
createReference :: Auth -> Name Owner -> Name Repo -> NewGitReference -> IO (Either Error GitReference)
createReference auth user repo newRef =
    executeRequest auth $ createReferenceR user repo newRef

-- | Create a reference.
-- See <https://developer.github.com/v3/git/refs/#create-a-reference>
createReferenceR :: Name Owner -> Name Repo -> NewGitReference -> Request 'RW GitReference
createReferenceR user repo newRef =
     command Post  ["repos", toPathPart user, toPathPart repo , "git", "refs"] (encode newRef)

-- | Limited references by a namespace.
--
-- > namespacedReferences "thoughtbot" "paperclip" "tags"
namespacedReferences :: Name Owner -> Name Repo -> Text -> IO (Either Error [GitReference])
namespacedReferences user repo namespace =
    executeRequest' $ namespacedReferencesR user repo namespace

-- | Query namespaced references.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
namespacedReferencesR :: Name Owner -> Name Repo -> Text -> Request k [GitReference]
namespacedReferencesR user repo namespace =
    query ["repos", toPathPart user, toPathPart repo, "git", "refs", namespace] []
