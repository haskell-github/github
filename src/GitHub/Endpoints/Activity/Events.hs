-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The events API as described on <https://developer.github.com/v3/activity/events/>.
module GitHub.Endpoints.Activity.Events (
    -- * Events
    repositoryEventsR,
    userEventsR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List repository events.
-- See <https://developer.github.com/v3/activity/events/#list-repository-events>
repositoryEventsR :: Name Owner -> Name Repo -> FetchCount -> Request 'RO (Vector Event)
repositoryEventsR user repo =
    pagedQuery  ["repos", toPathPart user, toPathPart repo, "events"] []

-- | List user public events.
-- See <https://developer.github.com/v3/activity/events/#list-public-events-performed-by-a-user>
userEventsR :: Name User -> FetchCount -> Request 'RO (Vector Event)
userEventsR user =
    pagedQuery ["users", toPathPart user, "events", "public"] []
