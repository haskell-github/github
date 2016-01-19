{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The underlying git references on a Github repo, exposed for the world to
-- see. The git internals documentation will also prove handy for understanding
-- these. API documentation at <http://developer.github.com/v3/git/refs/>.
module Github.GitData.References (
    reference,
    reference',
    referenceR,
    references,
    references',
    referencesR,
    createReference,
    createReferenceR,
    namespacedReferences,
    module Github.Data,
    ) where

import Data.Aeson.Compat (encode)
import Data.Vector       (Vector)

import Github.Data
import Github.Request

-- | A single reference by the ref name.
--
-- > reference' (Just ("github-username", "github-password")) "mike-burns" "github" "heads/master"
reference' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name GitReference -> IO (Either Error GitReference)
reference' auth user repo ref =
    executeRequestMaybe auth $ referenceR user repo ref

-- | A single reference by the ref name.
--
-- > reference "mike-burns" "github" "heads/master"
reference :: Name GithubOwner -> Name Repo -> Name GitReference -> IO (Either Error GitReference)
reference = reference' Nothing

-- | Get a reference.
-- See <https://developer.github.com/v3/git/refs/#get-a-reference>
referenceR :: Name GithubOwner -> Name Repo -> Name GitReference -> GithubRequest k GitReference
referenceR user repo ref =
    GithubGet ["repos", toPathPart user, toPathPart repo, "git", "refs", toPathPart ref] []

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector GitReference))
references' auth user repo =
    executeRequestMaybe auth $ referencesR user repo Nothing

-- | The history of references for a repo.
--
-- > references "mike-burns" "github"
references :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector GitReference))
references = references' Nothing

-- | Get all References.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
referencesR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector GitReference)
referencesR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "git", "refs"] []

-- | Create a reference.
createReference :: GithubAuth -> Name GithubOwner -> Name Repo -> NewGitReference -> IO (Either Error GitReference)
createReference auth user repo newRef =
    executeRequest auth $ createReferenceR user repo newRef

-- | Create a reference.
-- See <https://developer.github.com/v3/git/refs/#create-a-reference>
createReferenceR :: Name GithubOwner -> Name Repo -> NewGitReference -> GithubRequest 'True GitReference
createReferenceR user repo newRef =
     GithubPost Post  ["repos", toPathPart user, toPathPart repo , "git", "refs"] (encode newRef)

-- | Limited references by a namespace.
--
-- > namespacedReferences "thoughtbot" "paperclip" "tags"
namespacedReferences :: Name GithubOwner -> Name Repo -> String -> IO (Either Error [GitReference])
namespacedReferences user repo namespace =
    executeRequest' $ namespacedReferencesR user repo namespace

-- | Get namespaced references.
-- See <https://developer.github.com/v3/git/refs/#get-all-references>
namespacedReferencesR :: Name GithubOwner -> Name Repo -> String -> GithubRequest k [GitReference]
namespacedReferencesR user repo namespace =
    GithubGet ["repos", toPathPart user, toPathPart repo, "git", "refs", namespace] []
