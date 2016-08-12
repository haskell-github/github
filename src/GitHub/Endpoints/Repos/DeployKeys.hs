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
) where

import GitHub.Data
import GitHub.Request
import GitHub.Internal.Prelude

-- * Querying deploy keys
deployKeysFor' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector RepoDeployKey))
deployKeysFor' auth user repo =
    executeRequest auth $ deployKeysForR user repo FetchAll

deployKeysForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector RepoDeployKey)
deployKeysForR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "keys"] []

deployKeyFor' :: Auth -> Name Owner -> Name Repo -> Id RepoDeployKey -> IO (Either Error RepoDeployKey)
deployKeyFor' auth user repo keyId =
    executeRequest auth $ deployKeyForR user repo keyId

deployKeyForR :: Name Owner -> Name Repo -> Id RepoDeployKey -> Request k RepoDeployKey
deployKeyForR user repo keyId =
    Query ["repos", toPathPart user, toPathPart repo, "keys", toPathPart keyId] []
