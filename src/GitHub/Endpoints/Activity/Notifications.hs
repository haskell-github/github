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

markNotificationAsRead :: Auth -> Id Notification -> IO (Either Error ())
markNotificationAsRead auth notificationId =
  executeRequest auth $ markNotificationAsReadR notificationId

markNotificationAsReadR :: Id Notification -> Request 'RW ()
markNotificationAsReadR notificationId = SimpleQuery $
  Command Patch ["notifications", "threads", toPathPart notificationId]
            (encode ())

markNotificationsAsRead :: Auth -> IO (Either Error ())
markNotificationsAsRead auth =
  executeRequest auth markAllNotificationsAsReadR

markAllNotificationsAsReadR :: Request 'RW ()
markAllNotificationsAsReadR = SimpleQuery $
  Command Put ["notifications"] $ encode ()
