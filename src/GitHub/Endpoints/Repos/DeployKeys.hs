-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
-- The deploy keys API, as described at
-- <https://developer.github.com/v3/repos/keys>
module GitHub.Endpoints.Repos.DeployKeys (
    -- * Querying deploy keys
    deployKeysForR,
    deployKeyForR,

    -- ** Create
    createRepoDeployKeyR,

    -- ** Delete
    deleteRepoDeployKeyR,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Querying deploy keys.
-- See <https://developer.github.com/v3/repos/keys/#list-deploy-keys>
deployKeysForR :: Name Owner -> Name Repo -> FetchCount -> Request 'RA (Vector RepoDeployKey)
deployKeysForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "keys"] []

-- | Querying a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#get-a-deploy-key>
deployKeyForR :: Name Owner -> Name Repo -> Id RepoDeployKey -> Request 'RA RepoDeployKey
deployKeyForR user repo keyId =
    query ["repos", toPathPart user, toPathPart repo, "keys", toPathPart keyId] []

-- | Create a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#add-a-new-deploy-key>.
createRepoDeployKeyR :: Name Owner -> Name Repo -> NewRepoDeployKey -> Request 'RW RepoDeployKey
createRepoDeployKeyR user repo key =
    command Post ["repos", toPathPart user, toPathPart repo, "keys"] (encode key)

-- | Delete a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#remove-a-deploy-key>
deleteRepoDeployKeyR :: Name Owner -> Name Repo -> Id RepoDeployKey -> GenRequest 'MtUnit 'RW ()
deleteRepoDeployKeyR user repo keyId =
    Command Delete ["repos", toPathPart user, toPathPart repo, "keys", toPathPart keyId] mempty
