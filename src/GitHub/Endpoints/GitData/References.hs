-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module GitHub.Endpoints.GitData.References (
    referenceR,
    referencesR,
    createReferenceR,
    namespacedReferencesR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | A single reference -- | Query a reference.
-- See <https://developer.github.com/v3/git/refs/#get-a-reference>
referenceR :: Name Owner -> Name Repo -> Name GitReference -> Request k GitReference
referenceR user repo ref =
    query ["repos", toPathPart user, toPathPart repo, "git", "refs", toPathPart ref] []

-- | Query all References.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
referencesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector GitReference)
referencesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "git", "refs"] []

-- | Create a reference.
-- See <https://developer.github.com/v3/git/refs/#create-a-reference>
createReferenceR :: Name Owner -> Name Repo -> NewGitReference -> Request 'RW GitReference
createReferenceR user repo newRef =
     command Post  ["repos", toPathPart user, toPathPart repo , "git", "refs"] (encode newRef)

-- | Query namespaced references.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
namespacedReferencesR :: Name Owner -> Name Repo -> Text -> Request k [GitReference]
namespacedReferencesR user repo namespace =
    query ["repos", toPathPart user, toPathPart repo, "git", "refs", namespace] []
