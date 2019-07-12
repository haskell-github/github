-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Events where

import GitHub.Data.Definitions
import GitHub.Internal.Prelude
import Prelude ()

-- | Events.
--
-- /TODO:/
--
-- * missing org, payload
data Event = Event
    { eventActor     :: !SimpleUser
    , eventCreatedAt :: !UTCTime
    , eventPublic    :: !Bool
    , eventRepo      :: !SimpleRepo
    , eventId        :: !Text
    , eventType      :: !ActivityEvent
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Event where rnf = genericRnf
instance Binary Event

instance FromJSON Event where
    parseJSON = withObject "Event" $ \obj -> Event
        <$> obj .: "actor"
        <*> obj .: "created_at"
        <*> obj .: "public"
        <*> obj .: "repo"
        <*> obj .: "id"
        <*> obj .: "type"

-- See <https://developer.github.com/v3/activity/events/types/>
data ActivityEvent =
      CheckRunEvent
    | CheckSuiteEvent
    | CommitCommentEvent
    | ContentReferenceEvent
    | CreateEvent
    | DeleteEvent
    | DeployKeyEvent
    | DeploymentEvent
    | DeploymentStatusEvent
    | DownloadEvent
    | FollowEvent
    | ForkEvent
    | ForkApplyEvent
    | GitHubAppAuthorizationEvent
    | GistEvent
    | GollumEvent
    | InstallationEvent
    | InstallationRepositoriesEvent
    | IssueCommentEvent
    | IssuesEvent
    | LabelEvent
    | MarketplacePurchaseEvent
    | MemberEvent
    | MembershipEvent
    | MetaEvent
    | MilestoneEvent
    | OrganizationEvent
    | OrgBlockEvent
    | PageBuildEvent
    | ProjectCardEvent
    | ProjectColumnEvent
    | ProjectEvent
    | PublicEvent
    | PullRequestEvent'
    | PullRequestReviewEvent
    | PullRequestReviewCommentEvent
    | PushEvent
    | RegistryPackageEvent
    | ReleaseEvent
    | RepositoryEvent
    | RepositoryImportEvent
    | RepositoryVulnerabilityAlertEvent
    | SecurityAdvisoryEvent
    | StarEvent
    | StatusEvent
    | TeamEvent
    | TeamAddEvent
    | WatchEvent
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON ActivityEvent
instance NFData ActivityEvent where rnf = genericRnf
instance Binary ActivityEvent
