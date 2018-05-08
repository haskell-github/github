-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Activities where

import GitHub.Data.Id          (Id, mkId)
import GitHub.Data.Repos       (Repo, RepoRef)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude

import Prelude ()

data RepoStarred = RepoStarred
    { repoStarredStarredAt :: !UTCTime
    , repoStarredRepo      :: !Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoStarred where rnf = genericRnf
instance Binary RepoStarred

-- JSON Instances
instance FromJSON RepoStarred where
    parseJSON = withObject "RepoStarred" $ \o -> RepoStarred
        <$> o .: "starred_at"
        <*> o .: "repo"

data Subject = Subject
    { subjectTitle :: !Text
    , subjectURL :: !URL
    , subjectLatestCommentURL :: !(Maybe URL)
    -- https://developer.github.com/v3/activity/notifications/ doesn't indicate
    -- what the possible values for this field are.
    -- TODO: Make an ADT for this.
    , subjectType :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Subject where rnf = genericRnf
instance Binary Subject

instance FromJSON Subject where
    parseJSON = withObject "Subject" $ \o -> Subject
        <$> o .: "title"
        <*> o .: "url"
        <*> o .:? "latest_comment_url"
        <*> o .: "type"

data NotificationReason
    = AssignReason
    | AuthorReason
    | CommentReason
    | InvitationReason
    | ManualReason
    | MentionReason
    | StateChangeReason
    | SubscribedReason
    | TeamMentionReason
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData NotificationReason where rnf = genericRnf
instance Binary NotificationReason

instance FromJSON NotificationReason where
    parseJSON = withText "NotificationReason" $ \t -> case t of
        "assign" -> pure AssignReason
        "author" -> pure AuthorReason
        "comment" -> pure CommentReason
        "invitation" -> pure InvitationReason
        "manual" -> pure ManualReason
        "mention" -> pure MentionReason
        "state_change" -> pure StateChangeReason
        "subscribed" -> pure SubscribedReason
        "team_mention" -> pure TeamMentionReason
        _ -> fail $ "Unknown NotificationReason " ++ show t

data Notification = Notification
    -- XXX: The notification id field type IS in fact string. Not sure why gh
    -- chose to do this when all the other ids are Numbers...
    { notificationId :: !(Id Notification)
    , notificationRepo :: !RepoRef
    , notificationSubject :: !Subject
    , notificationReason :: !NotificationReason
    , notificationUnread :: !Bool
    , notificationUpdatedAt :: !(Maybe UTCTime)
    , notificationLastReadAt :: !(Maybe UTCTime)
    , notificationUrl :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Notification where rnf = genericRnf
instance Binary Notification

instance FromJSON Notification where
    parseJSON = withObject "Notification" $ \o -> Notification
        <$> (mkId undefined . read <$> o .: "id")
        <*> o .: "repository"
        <*> o .: "subject"
        <*> o .: "reason"
        <*> o .: "unread"
        <*> o .: "updated_at"
        <*> o .: "last_read_at"
        <*> o .: "url"

