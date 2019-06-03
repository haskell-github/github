-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo watching API as described on
-- <https://developer.github.com/v3/activity/notifications/>.

module GitHub.Endpoints.Activity.Notifications (
    getNotificationsR,
    markNotificationAsReadR,
    markAllNotificationsAsReadR,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List your notifications.
-- See <https://developer.github.com/v3/activity/notifications/#list-your-notifications>
getNotificationsR :: FetchCount -> Request 'RA (Vector Notification)
getNotificationsR = pagedQuery ["notifications"] []

-- | Mark a thread as read.
-- See <https://developer.github.com/v3/activity/notifications/#mark-a-thread-as-read>
markNotificationAsReadR :: Id Notification -> GenRequest 'MtUnit 'RW ()
markNotificationAsReadR nid = Command
    Patch
    ["notifications", "threads", toPathPart nid]
    (encode ())

-- | Mark as read.
-- See <https://developer.github.com/v3/activity/notifications/#mark-as-read>
markAllNotificationsAsReadR :: GenRequest 'MtUnit 'RW ()
markAllNotificationsAsReadR =
    Command Put ["notifications"] $ encode emptyObject

deleteThreadSubscription :: Auth -> Id Notification -> IO (Either Error ())
deleteThreadSubscription auth nid =
    executeRequest auth $ deleteThreadSubscriptionR nid

-- | Delete a thread subscription.
-- See <https://developer.github.com/v3/activity/notifications/#delete-a-thread-subscription>
deleteThreadSubscriptionR :: Id Notification -> GenRequest 'MtUnit 'RW ()
deleteThreadSubscriptionR nid = Command
    Delete
    ["notifications", "threads", toPathPart nid, "subscription"]
    mempty
