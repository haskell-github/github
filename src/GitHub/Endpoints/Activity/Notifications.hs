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

-- | List your notifications.
-- See <https://developer.github.com/v3/activity/notifications/#list-your-notifications>
getNotificationsR :: FetchCount -> Request 'RA (Vector Notification)
getNotificationsR = pagedQuery ["notifications"] []

markNotificationAsRead :: Auth -> Id Notification -> IO (Either Error ())
markNotificationAsRead auth nid =
    executeRequest auth $ markNotificationAsReadR nid

-- | Mark a thread as read.
-- See <https://developer.github.com/v3/activity/notifications/#mark-a-thread-as-read>
markNotificationAsReadR :: Id Notification -> GenRequest 'MtUnit 'RW ()
markNotificationAsReadR nid = Command
    Patch
    ["notifications", "threads", toPathPart nid]
    (encode ())

markNotificationsAsRead :: Auth -> IO (Either Error ())
markNotificationsAsRead auth =
  executeRequest auth markAllNotificationsAsReadR

-- | Mark as read.
-- See <https://developer.github.com/v3/activity/notifications/#mark-as-read>
markAllNotificationsAsReadR :: GenRequest 'MtUnit 'RW ()
markAllNotificationsAsReadR =
    Command Put ["notifications"] $ encode emptyObject
