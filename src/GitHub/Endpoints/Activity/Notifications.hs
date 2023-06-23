-- |
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
