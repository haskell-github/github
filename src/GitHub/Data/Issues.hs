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
import Prelude                  ()

data Issue = Issue
    { issueClosedAt    :: !(Maybe UTCTime)
    , issueUpdatedAt   :: !UTCTime
    , issueEventsUrl   :: !URL
    , issueHtmlUrl     :: !(Maybe URL)
    , issueClosedBy    :: !(Maybe SimpleUser)
    , issueLabels      :: !(Vector IssueLabel)
    , issueNumber      :: !IssueNumber
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
    , newIssueAssignees :: !(Vector (Name User))
    , newIssueMilestone :: !(Maybe (Id Milestone))
    , newIssueLabels    :: !(Maybe (Vector (Name IssueLabel)))
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewIssue where rnf = genericRnf
instance Binary NewIssue

data EditIssue = EditIssue
    { editIssueTitle     :: !(Maybe Text)
    , editIssueBody      :: !(Maybe Text)
    , editIssueAssignees :: !(Maybe (Vector (Name User)))
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

-- | See <https://developer.github.com/v3/issues/events/#events-1>
data EventType
    = Mentioned             -- ^ The actor was @mentioned in an issue body.
    | Subscribed            -- ^ The actor subscribed to receive notifications for an issue.
    | Unsubscribed          -- ^ The issue was unsubscribed from by the actor.
    | Referenced            -- ^ The issue was referenced from a commit message. The commit_id attribute is the commit SHA1 of where that happened.
    | Merged                -- ^ The issue was merged by the actor. The commit_id attribute is the SHA1 of the HEAD commit that was merged.
    | Assigned              -- ^ The issue was assigned to the actor.
    | Closed                -- ^ The issue was closed by the actor. When the commit_id is present, it identifies the commit that closed the issue using “closes / fixes #NN” syntax.
    | Reopened              -- ^ The issue was reopened by the actor.
    | ActorUnassigned       -- ^ The issue was unassigned to the actor
    | Labeled               -- ^ A label was added to the issue.
    | Unlabeled             -- ^ A label was removed from the issue.
    | Milestoned            -- ^ The issue was added to a milestone.
    | Demilestoned          -- ^ The issue was removed from a milestone.
    | Renamed               -- ^ The issue title was changed.
    | Locked                -- ^ The issue was locked by the actor.
    | Unlocked              -- ^ The issue was unlocked by the actor.
    | HeadRefDeleted        -- ^ The pull request’s branch was deleted.
    | HeadRefRestored       -- ^ The pull request’s branch was restored.
    | ReviewRequested       -- ^ The actor requested review from the subject on this pull request.
    | ReviewDismissed       -- ^ The actor dismissed a review from the pull request.
    | ReviewRequestRemoved  -- ^ The actor removed the review request for the subject on this pull request.
    | MarkedAsDuplicate     -- ^ A user with write permissions marked an issue as a duplicate of another issue or a pull request as a duplicate of another pull request.
    | UnmarkedAsDuplicate   -- ^ An issue that a user had previously marked as a duplicate of another issue is no longer considered a duplicate, or a pull request that a user had previously marked as a duplicate of another pull request is no longer considered a duplicate.
    | AddedToProject        -- ^ The issue was added to a project board.
    | MovedColumnsInProject -- ^ The issue was moved between columns in a project board.
    | RemovedFromProject    -- ^ The issue was removed from a project board.
    | ConvertedNoteToIssue  -- ^ The issue was created by converting a note in a project board to an issue.
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData EventType where rnf = genericRnf
instance Binary EventType

-- | Issue event
data IssueEvent = IssueEvent
    { issueEventActor     :: !SimpleUser
    , issueEventType      :: !EventType
    , issueEventCommitId  :: !(Maybe Text)
    , issueEventUrl       :: !URL
    , issueEventCreatedAt :: !UTCTime
    , issueEventId        :: !Int
    , issueEventIssue     :: !(Maybe Issue)
    , issueEventLabel     :: !(Maybe IssueLabel)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueEvent where rnf = genericRnf
instance Binary IssueEvent

instance FromJSON IssueEvent where
    parseJSON = withObject "Event" $ \o -> IssueEvent
        <$> o .: "actor"
        <*> o .: "event"
        <*> o .:? "commit_id"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .:? "issue"
        <*> o .:? "label"

instance FromJSON EventType where
    parseJSON = withText "EventType" $ \t -> case t of
        "closed"                   -> pure Closed
        "reopened"                 -> pure Reopened
        "subscribed"               -> pure Subscribed
        "merged"                   -> pure Merged
        "referenced"               -> pure Referenced
        "mentioned"                -> pure Mentioned
        "assigned"                 -> pure Assigned
        "unassigned"               -> pure ActorUnassigned
        "labeled"                  -> pure Labeled
        "unlabeled"                -> pure Unlabeled
        "milestoned"               -> pure Milestoned
        "demilestoned"             -> pure Demilestoned
        "renamed"                  -> pure Renamed
        "locked"                   -> pure Locked
        "unlocked"                 -> pure Unlocked
        "head_ref_deleted"         -> pure HeadRefDeleted
        "head_ref_restored"        -> pure HeadRefRestored
        "review_requested"         -> pure ReviewRequested
        "review_dismissed"         -> pure ReviewDismissed
        "review_request_removed"   -> pure ReviewRequestRemoved
        "marked_as_duplicate"      -> pure MarkedAsDuplicate
        "unmarked_as_duplicate"    -> pure UnmarkedAsDuplicate
        "added_to_project"         -> pure AddedToProject
        "moved_columns_in_project" -> pure MovedColumnsInProject
        "removed_from_project"     -> pure RemovedFromProject
        "converted_note_to_issue"  -> pure ConvertedNoteToIssue
        "unsubscribed"             -> pure Unsubscribed -- not in api docs list
        _                          -> fail $ "Unknown EventType " ++ show t

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
    toJSON (NewIssue t b a m ls) = object $ filter notNull
        [ "title"     .= t
        , "body"      .= b
        , "assignees" .= a
        , "milestone" .= m
        , "labels"    .= ls
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True

instance ToJSON EditIssue where
    toJSON (EditIssue t b a s m ls) = object $ filter notNull
        [ "title"     .= t
        , "body"      .= b
        , "assignees" .= a
        , "state"     .= s
        , "milestone" .= m
        , "labels"    .= ls
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True
