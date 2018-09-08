{-# LANGUAGE LambdaCase #-}

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

deploymentStatusesForR
    :: Name Owner
    -> Name Repo
    -> Id (Deployment a)
    -> FetchCount
    -> Request 'RA (Vector DeploymentStatus)
deploymentStatusesForR owner repo deploy =
    pagedQuery (statusesPaths owner repo deploy) []

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
