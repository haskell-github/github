-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Issues where

import GitHub.Data.Definitions
import GitHub.Data.Id           (Id)
import GitHub.Data.Milestone    (Milestone)
import GitHub.Data.Name         (Name)
import GitHub.Data.Options      (IssueState)
import GitHub.Data.PullRequests
import GitHub.Data.URL          (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Issue = Issue
    { issueClosedAt    :: !(Maybe UTCTime)
    , issueUpdatedAt   :: !UTCTime
    , issueEventsUrl   :: !URL
    , issueHtmlUrl     :: !(Maybe URL)
    , issueClosedBy    :: !(Maybe SimpleUser)
    , issueLabels      :: (Vector IssueLabel)
    , issueNumber      :: !Int
    , issueAssignees   :: !(Vector SimpleUser)
    , issueUser        :: !SimpleUser
    , issueTitle       :: !Text
    , issuePullRequest :: !(Maybe PullRequestReference)
    , issueUrl         :: !URL
    , issueCreatedAt   :: !UTCTime
    , issueBody        :: !(Maybe Text)
    , issueState       :: !IssueState
    , issueId          :: !(Id Issue)
    , issueComments    :: !Int
    , issueMilestone   :: !(Maybe Milestone)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Issue where rnf = genericRnf
instance Binary Issue

data NewIssue = NewIssue
    { newIssueTitle     :: !Text
    , newIssueBody      :: !(Maybe Text)
    , newIssueAssignee  :: !(Maybe Text)
    , newIssueMilestone :: !(Maybe (Id Milestone))
    , newIssueLabels    :: !(Maybe (Vector (Name IssueLabel)))
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewIssue where rnf = genericRnf
instance Binary NewIssue

data EditIssue = EditIssue
    { editIssueTitle     :: !(Maybe Text)
    , editIssueBody      :: !(Maybe Text)
    , editIssueAssignee  :: !(Maybe (Name User))
    , editIssueState     :: !(Maybe IssueState)
    , editIssueMilestone :: !(Maybe (Id Milestone))
    , editIssueLabels    :: !(Maybe (Vector (Name IssueLabel)))
    }
  deriving  (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditIssue where rnf = genericRnf
instance Binary EditIssue

data IssueComment = IssueComment
    { issueCommentUpdatedAt :: !UTCTime
    , issueCommentUser      :: !SimpleUser
    , issueCommentUrl       :: !URL
    , issueCommentHtmlUrl   :: !URL
    , issueCommentCreatedAt :: !UTCTime
    , issueCommentBody      :: !Text
    , issueCommentId        :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueComment where rnf = genericRnf
instance Binary IssueComment

data EventType
    = Mentioned        -- ^ The actor was @mentioned in an issue body.
    | Subscribed       -- ^ The actor subscribed to receive notifications for an issue.
    | Unsubscribed     -- ^ The issue was unsubscribed from by the actor.
    | Referenced       -- ^ The issue was referenced from a commit message. The commit_id attribute is the commit SHA1 of where that happened.
    | Merged           -- ^ The issue was merged by the actor. The commit_id attribute is the SHA1 of the HEAD commit that was merged.
    | Assigned         -- ^ The issue was assigned to the actor.
    | Closed           -- ^ The issue was closed by the actor. When the commit_id is present, it identifies the commit that closed the issue using “closes / fixes #NN” syntax.
    | Reopened         -- ^ The issue was reopened by the actor.
    | ActorUnassigned  -- ^ The issue was unassigned to the actor
    | Labeled          -- ^ A label was added to the issue.
    | Unlabeled        -- ^ A label was removed from the issue.
    | Milestoned       -- ^ The issue was added to a milestone.
    | Demilestoned     -- ^ The issue was removed from a milestone.
    | Renamed          -- ^ The issue title was changed.
    | Locked           -- ^ The issue was locked by the actor.
    | Unlocked         -- ^ The issue was unlocked by the actor.
    | HeadRefDeleted   -- ^ The pull request’s branch was deleted.
    | HeadRefRestored  -- ^ The pull request’s branch was restored.
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData EventType where rnf = genericRnf
instance Binary EventType

-- | Issue event
data Event = Event
    { eventActor     :: !SimpleUser
    , eventType      :: !EventType
    , eventCommitId  :: !(Maybe Text)
    , eventUrl       :: !URL
    , eventCreatedAt :: !UTCTime
    , eventId        :: !Int
    , eventIssue     :: !(Maybe Issue)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Event where rnf = genericRnf
instance Binary Event

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> Event
        <$> o .: "actor"
        <*> o .: "event"
        <*> o .:? "commit_id"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .:? "issue"

instance FromJSON EventType where
    parseJSON (String "closed") = pure Closed
    parseJSON (String "reopened") = pure Reopened
    parseJSON (String "subscribed") = pure Subscribed
    parseJSON (String "merged") = pure Merged
    parseJSON (String "referenced") = pure Referenced
    parseJSON (String "mentioned") = pure Mentioned
    parseJSON (String "assigned") = pure Assigned
    parseJSON (String "unsubscribed") = pure Unsubscribed
    parseJSON (String "unassigned") = pure ActorUnassigned
    parseJSON (String "labeled") = pure Labeled
    parseJSON (String "unlabeled") = pure Unlabeled
    parseJSON (String "milestoned") = pure Milestoned
    parseJSON (String "demilestoned") = pure Demilestoned
    parseJSON (String "renamed") = pure Renamed
    parseJSON (String "locked") = pure Locked
    parseJSON (String "unlocked") = pure Unlocked
    parseJSON (String "head_ref_deleted") = pure HeadRefDeleted
    parseJSON (String "head_ref_restored") = pure HeadRefRestored
    parseJSON _ = fail "Could not build an EventType"

instance FromJSON IssueComment where
    parseJSON = withObject "IssueComment" $ \o -> IssueComment
        <$> o .: "updated_at"
        <*> o .: "user"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "created_at"
        <*> o .: "body"
        <*> o .: "id"

instance FromJSON Issue where
    parseJSON = withObject "Issue" $ \o -> Issue
        <$> o .:? "closed_at"
        <*> o .: "updated_at"
        <*> o .: "events_url"
        <*> o .: "html_url"
        <*> o .:? "closed_by"
        <*> o .: "labels"
        <*> o .: "number"
        <*> o .:? "assignee"
        <*> o .: "assignees"
        <*> o .: "user"
        <*> o .: "title"
        <*> o .:? "pull_request"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "body"
        <*> o .: "state"
        <*> o .: "id"
        <*> o .: "comments"
        <*> o .:? "milestone"

instance ToJSON NewIssue where
    toJSON (NewIssue t b a m ls) = object
        [ "title"     .= t
        , "body"      .= b
        , "assignee"  .= a
        , "milestone" .= m
        , "labels"    .= ls
        ]

instance ToJSON EditIssue where
    toJSON (EditIssue t b a s m ls) = object $ filter notNull $
        [ "title"     .= t
        , "body"      .= b
        , "assignee"  .= a
        , "state"     .= s
        , "milestone" .= m
        , "labels"    .= ls
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True
