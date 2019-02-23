-- | The deployments API, as described at <https://developer.github.com/v3/repos/deployments/>
module GitHub.Endpoints.Repos.Deployments
    ( deploymentsWithOptionsForR
    , createDeploymentR

    , deploymentStatusesForR
    , createDeploymentStatusR

    , module GitHub.Data
    ) where

import Control.Arrow (second)

import Data.Vector (Vector)

import GitHub.Data
import GitHub.Internal.Prelude

-- | List deployments.
-- See <https://developer.github.com/v3/repos/deployments/#list-deployments>
deploymentsWithOptionsForR
    :: FromJSON a
    => Name Owner
    -> Name Repo
    -> FetchCount
    -> [DeploymentQueryOption]
    -> Request 'RA (Vector (Deployment a))
deploymentsWithOptionsForR owner repo limit opts =
    pagedQuery (deployPaths owner repo)
        (map (second Just . renderDeploymentQueryOption) opts)
        limit

-- | Create a deployment.
-- See <https://developer.github.com/v3/repos/deployments/#create-a-deployment>
createDeploymentR
    :: ( ToJSON   a
       , FromJSON a
       )
    => Name Owner
    -> Name Repo
    -> CreateDeployment a
    -> Request 'RW (Deployment a)
createDeploymentR owner repo =
    command Post (deployPaths owner repo) . encode

-- | List deployment statuses.
-- See <https://developer.github.com/v3/repos/deployments/#list-deployment-statuses>
deploymentStatusesForR
    :: Name Owner
    -> Name Repo
    -> Id (Deployment a)
    -> FetchCount
    -> Request 'RA (Vector DeploymentStatus)
deploymentStatusesForR owner repo deploy =
    pagedQuery (statusesPaths owner repo deploy) []

-- | Create a deployment status.
-- See <https://developer.github.com/v3/repos/deployments/#list-deployment-statuses>
createDeploymentStatusR
    :: Name Owner
    -> Name Repo
    -> Id (Deployment a)
    -> CreateDeploymentStatus
    -> Request 'RW DeploymentStatus
createDeploymentStatusR owner repo deploy =
    command Post (statusesPaths owner repo deploy) . encode

statusesPaths :: Name Owner -> Name Repo -> Id (Deployment a) -> Paths
statusesPaths owner repo deploy =
    deployPaths owner repo ++ [toPathPart deploy, "statuses"]

deployPaths :: Name Owner -> Name Repo -> Paths
deployPaths owner repo =
    ["repos", toPathPart owner, toPathPart repo, "deployments"]
