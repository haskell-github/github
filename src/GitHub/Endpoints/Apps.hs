-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The GitHub Apps API, as described at
-- <https://developer.github.com/v3/apps/>.
module GitHub.Endpoints.Apps
    ( createAccessToken
    , createAccessTokenR
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | Create an access token for a given installation.
createAccessToken :: AppAuth -> Id Installation -> IO (Either Error AccessToken)
createAccessToken auth installation =
    executeRequest auth $ createAccessTokenR installation

-- | Create an access token for a given installation.
-- See <https://developer.github.com/v3/apps/#create-a-new-installation-token>
createAccessTokenR :: Id Installation -> GenRequest 'MtMachineManPreview 'RW AccessToken
createAccessTokenR installation =
    Command Post ["app", "installations", toPathPart installation, "access_tokens"] mempty
