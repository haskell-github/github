-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
-- The deploy keys API, as described at
-- <https://developer.github.com/v3/repos/keys>
module GitHub.Endpoints.Repos.DeployKeys (
    -- * Querying deploy keys
    deployKeysFor',
    deployKeysForR,
    deployKeyFor',
    deployKeyForR,

    -- ** Create
    createRepoDeployKey',
    createRepoDeployKeyR,

    -- ** Delete
    deleteRepoDeployKey',
    deleteRepoDeployKeyR,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | Querying deploy keys.
deployKeysFor' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector RepoDeployKey))
deployKeysFor' auth user repo =
    executeRequest auth $ deployKeysForR user repo FetchAll

-- | Querying deploy keys.
-- See <https://developer.github.com/v3/repos/keys/#list-deploy-keys>
deployKeysForR :: Name Owner -> Name Repo -> FetchCount -> Request 'RA (Vector RepoDeployKey)
deployKeysForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "keys"] []

-- | Querying a deploy key
deployKeyFor' :: Auth -> Name Owner -> Name Repo -> Id RepoDeployKey -> IO (Either Error RepoDeployKey)
deployKeyFor' auth user repo keyId =
    executeRequest auth $ deployKeyForR user repo keyId

-- | Querying a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#get-a-deploy-key>
deployKeyForR :: Name Owner -> Name Repo -> Id RepoDeployKey -> Request 'RA RepoDeployKey
deployKeyForR user repo keyId =
    query ["repos", toPathPart user, toPathPart repo, "keys", toPathPart keyId] []

-- | Create a deploy key
createRepoDeployKey' :: Auth -> Name Owner -> Name Repo -> NewRepoDeployKey -> IO (Either Error RepoDeployKey)
createRepoDeployKey' auth user repo key =
    executeRequest auth $ createRepoDeployKeyR user repo key

-- | Create a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#add-a-new-deploy-key>.
createRepoDeployKeyR :: Name Owner -> Name Repo -> NewRepoDeployKey -> Request 'RW RepoDeployKey
createRepoDeployKeyR user repo key =
    command Post ["repos", toPathPart user, toPathPart repo, "keys"] (encode key)

deleteRepoDeployKey' :: Auth -> Name Owner -> Name Repo -> Id RepoDeployKey -> IO (Either Error ())
deleteRepoDeployKey' auth user repo keyId =
    executeRequest auth $ deleteRepoDeployKeyR user repo keyId

-- | Delete a deploy key.
-- See <https://developer.github.com/v3/repos/keys/#remove-a-deploy-key>
deleteRepoDeployKeyR :: Name Owner -> Name Repo -> Id RepoDeployKey -> Request 'RW ()
deleteRepoDeployKeyR user repo keyId =
    command Delete ["repos", toPathPart user, toPathPart repo, "keys", toPathPart keyId] mempty
