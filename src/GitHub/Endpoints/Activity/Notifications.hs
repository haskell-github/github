-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo watching API as described on
-- <https://developer.github.com/v3/activity/notifications/>.

module GitHub.Endpoints.Activity.Notifications where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

getNotifications :: Auth -> IO (Either Error (Vector Notification))
getNotifications auth =
  executeRequest auth $ getNotificationsR FetchAll

getNotificationsR :: FetchCount -> Request 'RA (Vector Notification)
getNotificationsR =
  pagedQuery ["notifications"] []
