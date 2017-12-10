-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Payloads describing the structure of the Webhook events.
-- For smaller components of payloads see `GitHub.Data.Webhooks.Payload`.

module GitHub.Data.Webhooks.Events
    ( WebHookEvent(..)
      --
    , CommitCommentEvent(..)
    , CommitCommentEventAction
    , commitCommentCreated
      --
    , CreateEvent(..)
      --
    , DeleteEvent(..)
      --
    , DeploymentEvent(..)
      --
    , DeploymentStatusEvent(..)
      -- Deprecated events
    , DownloadEvent(..)
    , FollowEvent(..)
    , ForkEvent(..)
    , ForkApplyEvent(..)
    , GistEvent(..)
      --
    , GollumEvent(..)
      --
    , InstallationEvent(..)
    , InstallationEventAction
    , installationCreated
    , installationDeleted
      --
    , InstallationRepositoriesEvent(..)
    , InstallationRepoEventAction
    , installationRepoCreated
    , installationRepoDeleted
      --
    , IssueCommentEvent(..)
    , IssueCommentEventAction
    , issueCommentCreated
    , issueCommentEdited
    , issueCommentDeleted
      --
    , IssuesEvent(..)
    , IssuesEventAction
    , issuesAssigned
    , issuesUnassigned
    , issuesLabeled
    , issuesUnlabeled
    , issuesOpened
    , issuesEdited
    , issuesMilestoned
    , issuesDemilestoned
    , issuesClosed
    , issuesReopened
      --
    , LabelEvent(..)
    , LabelEventAction
    , labelCreated
    , labelEdited
    , labelDeleted
      --
    , MemberEvent(..)
    , MemberEventAction
    , memberAdded
    , memberDeleted
    , memberEdited
      --
    , MembershipEvent(..)
    , MembershipEventAction
    , membershipAdded
    , membershipRemoved
      --
    , MilestoneEvent(..)
    , MilestoneEventAction
    , milestoneCreated
    , milestoneClosed
    , milestoneOpened
    , milestoneEdited
    , milestoneDeleted
      --
    , OrganizationEvent(..)
    , OrganizationEventAction
    , organizationMemberAdded
    , organizationMemberRemoved
    , organizationMemberInvited
      --
    , OrgBlockEvent(..)
    , OrgBlockEventAction
    , orgBlocksUser
    , orgUnblocksUser
      --
    , PageBuildEvent(..)
      --
    , ProjectCardEvent(..)
    , ProjectCardEventAction
    , projectCardCreated
    , projectCardEdited
    , projectCardConverted
    , projectCardMoved
    , projectCardDeleted
      --
    , ProjectColumnEvent(..)
    , ProjectColumnEventAction
    , projectColumnCreated
    , projectColumnEdited
    , projectColumnMoved
    , projectColumnDeleted
      --
    , ProjectEvent(..)
    , ProjectEventAction
    , projectCreated
    , projectEdited
    , projectClosed
    , projectReopened
    , projectDeleted
      --
    , PublicEvent(..)
      --
    , PullRequestEvent(..)
    , PullRequestEventAction
    , pullRequestAssigned
    , pullRequestUnassigned
    , pullRequestReviewRequested
    , pullRequestReviewRequestRemoved
    , pullRequestLabeled
    , pullRequestUnlabeled
    , pullRequestOpened
    , pullRequestEdited
    , pullRequestClosed
    , pullRequestReopened
      --
    , PullRequestReviewEvent(..)
    , PullRequestReviewEventAction
    , pullRequestReviewSubmitted
    , pullRequestReviewEdited
    , pullRequestReviewDismissed
      --
    , PullRequestReviewCommentEvent(..)
    , PullRequestReviewCommentEventAction
    , pullRequestReviewCommentCreated
    , pullRequestReviewCommentEdited
    , pullRequestReviewCommentDeleted
      --
    , PushEvent(..)
      --
    , ReleaseEvent(..)
    , ReleaseEventAction
    , releasePublished
      --
    , RepositoryEvent(..)
    , RepositoryEventAction
    , repositoryCreated
    , repositoryDeleted
    , repositoryArchived
    , repositoryUnarchived
    , repositoryPublicized
    , repositoryPrivatized
      --
    , StatusEvent(..)
    , StatusEventState
    , gitCommitPending
    , gitCommitSuccess
    , gitCommitFailure
    , gitCommitError
      --
    , TeamEvent(..)
    , TeamEventAction
    , teamCreated
    , teamDeleted
    , teamEdited
    , teamAddedToRepo
    , teamRemovedFromRepo
      --
    , TeamAddEvent(..)
      --
    , WatchEvent(..)
    , WatchEventAction
    , startedWatching
    ) where

import GitHub.Internal.Prelude
import GitHub.Data.Definitions    (OwnerType)
import GitHub.Data.URL            (URL)
import GitHub.Data.Webhooks.Payload


class WebHookEvent eventKind where
    -- | Returns the sender of a Webhook event.
    senderOfEvent :: eventKind -> HookUser


type CommitCommentEventAction = Text

commitCommentCreated :: CommitCommentEventAction
commitCommentCreated = pack "created"

-- FIXME: add the rest of the smart constructors for evCommitCommentAction here.

-- | Triggered when a commit comment is created.
-- See <https://developer.github.com/v3/activity/events/types/#commitcommentevent>.
data CommitCommentEvent = CommitCommentEvent
    { evCommitCommentAction     :: !CommitCommentEventAction
    , evCommitCommentPayload    :: !HookCommitComment
    , evCommitCommentRepo       :: !HookRepository
    , evCommitCommentSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent CommitCommentEvent where senderOfEvent = evCommitCommentSender
instance NFData CommitCommentEvent where rnf = genericRnf


-- | Represents a created repository, branch, or tag.
-- Note: webhooks will not receive this event for created repositories.
-- Additionally, webhooks will not receive this event for tags if more than three tags are pushed at once.
-- See <https://developer.github.com/v3/activity/events/types/#createevent>.
data CreateEvent = CreateEvent
    { evCreateRef               :: !Text
    , evCreateRefType           :: !Text
    , evCreateMasterBranch      :: !Text
    , evCreateDescription       :: !Text
    , evCreatePusherType        :: !OwnerType
    , evCreateRepo              :: !HookRepository
    , evCreateSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent CreateEvent where senderOfEvent = evCreateSender
instance NFData CreateEvent where rnf = genericRnf


-- | Represents a deleted branch or tag.
-- Note: webhooks will not receive this event for tags if more than three tags are deleted at once.
-- See <https://developer.github.com/v3/activity/events/types/#deleteevent>.
data DeleteEvent = DeleteEvent
    { evDeleteRef               :: !Text
    , evDeleteRefType           :: !Text
    , evDeletePusherType        :: !OwnerType
    , evDeleteRepo              :: !HookRepository
    , evDeleteSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent DeleteEvent where senderOfEvent = evDeleteSender
instance NFData DeleteEvent where rnf = genericRnf


-- | Represents a deployment.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#deploymentevent>.
data DeploymentEvent = DeploymentEvent
    { evDeploymentInfo          :: !HookDeployment
    , evDeploymentRepo          :: !HookRepository
    , evDeploymentSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent DeploymentEvent where senderOfEvent = evDeploymentSender
instance NFData DeploymentEvent where rnf = genericRnf


-- | Represents a deployment status.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#deploymentstatusevent>.
data DeploymentStatusEvent = DeploymentStatusEvent
    { evDeplStatusInfo          :: !HookDeploymentStatus
    , evDeplStatusDeployment    :: !HookDeployment
    , evDeplStatusRepo          :: !HookRepository
    , evDeplStatusSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent DeploymentStatusEvent where senderOfEvent = evDeplStatusSender
instance NFData DeploymentStatusEvent where rnf = genericRnf


-- | Triggered when a new download is created.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#downloadevent>.
data DownloadEvent = DownloadEvent

-- | Triggered when a user follows another user.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#downloadevent>.
data FollowEvent = FollowEvent

-- | Triggered when a user forks a repository.
-- See <https://developer.github.com/v3/activity/events/types/#forkevent>.
data ForkEvent = ForkEvent
    { evForkDestination         :: !HookRepository
    , evForkSource              :: !HookRepository
    , evForkSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent ForkEvent where senderOfEvent = evForkSender
instance NFData ForkEvent where rnf = genericRnf


-- | Triggered when a patch is applied in the Fork Queue.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#forkapplyevent>.
data ForkApplyEvent = ForkApplyEvent


-- | Triggered when a Gist is created or updated.
-- Events of this kind are no longer delivered.
-- See <https://developer.github.com/v3/activity/events/types/#gistevent>.
data GistEvent = GistEvent


-- | Triggered when a Wiki page is created or updated.
-- See <https://developer.github.com/v3/activity/events/types/#gollumevent>.
data GollumEvent = GollumEvent
    { evGollumPages             :: !(Vector HookWikiPage)
    , evGollumRepo              :: !HookRepository
    , evGollumSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent GollumEvent where senderOfEvent = evGollumSender
instance NFData GollumEvent where rnf = genericRnf


type InstallationEventAction = Text

installationCreated :: InstallationEventAction
installationCreated = pack "created"

installationDeleted :: InstallationEventAction
installationDeleted = pack "deleted"

-- | Triggered when a GitHub App has been installed or uninstalled.
-- See <https://developer.github.com/v3/activity/events/types/#installationevent>.
data InstallationEvent = InstallationEvent
    { evInstallationAction      :: !InstallationEventAction
    , evInstallationInfo        :: !HookInstallation
    , evInstallationSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent InstallationEvent where senderOfEvent = evInstallationSender
instance NFData InstallationEvent where rnf = genericRnf


type InstallationRepoEventAction = Text

installationRepoCreated :: InstallationRepoEventAction
installationRepoCreated = pack "created"

installationRepoDeleted :: InstallationRepoEventAction
installationRepoDeleted = pack "deleted"

-- | Triggered when a repository is added or removed from an installation.
-- See <https://developer.github.com/v3/activity/events/types/#installationrepositoriesevent>.
data InstallationRepositoriesEvent = InstallationRepositoriesEvent
    { evInstallationRepoAction  :: !InstallationRepoEventAction
    , evInstallationRepoInfo    :: !HookInstallation
    , evInstallationRepoSel     :: !Text
    , evInstallationReposAdd    :: !(Vector HookRepository)
    , evInstallationReposRemove :: !(Vector HookRepository)
    , evInstallationReposSender :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent InstallationRepositoriesEvent where senderOfEvent = evInstallationReposSender
instance NFData InstallationRepositoriesEvent where rnf = genericRnf


type IssueCommentEventAction = Text

issueCommentCreated :: IssueCommentEventAction
issueCommentCreated = pack "created"

issueCommentEdited :: IssueCommentEventAction
issueCommentEdited = pack "edited"

issueCommentDeleted :: IssueCommentEventAction
issueCommentDeleted = pack "deleted"

-- | Triggered when an issue comment is created, edited, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#issuecommentevent>.
data IssueCommentEvent = IssueCommentEvent
    { evIssueCommentAction      :: !IssueCommentEventAction
    , evIssueCommentIssue       :: !HookIssue
    , evIssueCommentPayload     :: !HookIssueComment
    , evIssueCommentRepo        :: !HookRepository
    , evIssueCommentSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent IssueCommentEvent where senderOfEvent = evIssueCommentSender
instance NFData IssueCommentEvent where rnf = genericRnf


type IssuesEventAction = Text

issuesAssigned :: IssuesEventAction
issuesAssigned = pack "assigned"

issuesUnassigned :: IssuesEventAction
issuesUnassigned = pack "unassigned"

issuesLabeled :: IssuesEventAction
issuesLabeled = pack "labeled"

issuesUnlabeled :: IssuesEventAction
issuesUnlabeled = pack "unlabeled"

issuesOpened :: IssuesEventAction
issuesOpened = pack "opened"

issuesEdited :: IssuesEventAction
issuesEdited = pack "edited"

issuesMilestoned :: IssuesEventAction
issuesMilestoned = pack "milestoned"

issuesDemilestoned :: IssuesEventAction
issuesDemilestoned = pack "demilestoned"

issuesClosed :: IssuesEventAction
issuesClosed = pack "closed"

issuesReopened :: IssuesEventAction
issuesReopened = pack "reopened"

-- | Triggered when an issue is assigned, unassigned, labeled,
--  unlabeled, opened, edited, milestoned, demilestoned, closed, or reopened.
-- See <https://developer.github.com/v3/activity/events/types/#issuesevent>.
data IssuesEvent = IssuesEvent
    { evIssuesEventAction       :: !IssuesEventAction
    , evIssuesEventIssue        :: !HookIssue
    , evIssuesEventRepo         :: !HookRepository
    , evIssuesEventSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent IssuesEvent where senderOfEvent = evIssuesEventSender
instance NFData IssuesEvent where rnf = genericRnf


type LabelEventAction = Text

labelCreated :: LabelEventAction
labelCreated = pack "created"

labelEdited :: LabelEventAction
labelEdited = pack "edited"

labelDeleted :: LabelEventAction
labelDeleted = pack "deleted"

-- | Triggered when a repository's label is created, edited, or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#labelevent>.
data LabelEvent = LabelEvent
    { evLabelEventAction        :: !LabelEventAction
    , evLabelEventPayload       :: !HookRepositoryLabel
    , evLabelEventRepo          :: !HookRepository
    , evLabelEventOrganization  :: !(Maybe HookOrganization)
    , evLabelEventSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent LabelEvent where senderOfEvent = evLabelEventSender
instance NFData LabelEvent where rnf = genericRnf


type MemberEventAction = Text

memberAdded :: MemberEventAction
memberAdded = pack "added"

memberDeleted :: MemberEventAction
memberDeleted = pack "deleted"

memberEdited :: MemberEventAction
memberEdited = pack "edited"

-- | Triggered when a user is added or removed as a collaborator to a repository, or has their permissions changed.
-- See <https://developer.github.com/v3/activity/events/types/#memberevent>.
data MemberEvent = MemberEvent
    { evMemberAction            :: !MemberEventAction
    , evMemberUser              :: !HookUser
    , evMemberRepo              :: !HookRepository
    , evMemberSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent MemberEvent where senderOfEvent = evMemberSender
instance NFData MemberEvent where rnf = genericRnf


type MembershipEventAction = Text

membershipAdded :: MembershipEventAction
membershipAdded = pack "added"

membershipRemoved :: MembershipEventAction
membershipRemoved = pack "removed"

-- | Triggered when a user is added or removed from a team.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#membershipevent>.
data MembershipEvent = MembershipEvent
    { evMembershipAction        :: !MembershipEventAction
    , evMembershipScope         :: !Text        -- ^ Current can only be "team"
    , evMembershipUser          :: !HookUser
    , evMembershipTeam          :: !HookTeam
    , evMembershipOrg           :: !HookOrganization
    , evMembershipSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent MembershipEvent where senderOfEvent = evMembershipSender
instance NFData MembershipEvent where rnf = genericRnf


type MilestoneEventAction = Text

milestoneCreated :: MilestoneEventAction
milestoneCreated = pack "created"

milestoneClosed :: MilestoneEventAction
milestoneClosed = pack "closed"

milestoneOpened :: MilestoneEventAction
milestoneOpened = pack "opened"

milestoneEdited :: MilestoneEventAction
milestoneEdited = pack "edited"

milestoneDeleted :: MilestoneEventAction
milestoneDeleted = pack "deleted"

-- | Triggered when a milestone is created, closed, opened, edited, or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#milestoneevent>.
data MilestoneEvent = MilestoneEvent
    { evMilestoneAction         :: !MilestoneEventAction
    , evMilestoenPayload        :: !HookMilestone
    , evMilestoneRepo           :: !HookRepository
    , evMilestoneOrg            :: !HookOrganization
    , evMilestoneSender         :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent MilestoneEvent where senderOfEvent = evMilestoneSender
instance NFData MilestoneEvent where rnf = genericRnf


type OrganizationEventAction = Text

organizationMemberAdded :: OrganizationEventAction
organizationMemberAdded = pack "member_added"

organizationMemberRemoved :: OrganizationEventAction
organizationMemberRemoved = pack "member_removed"

organizationMemberInvited :: OrganizationEventAction
organizationMemberInvited = pack "member_invited"

-- | Triggered when a user is added, removed, or invited to an Organization.
-- Events of this type are not visible in timelines. These events are only used to trigger organization hooks.
-- See <https://developer.github.com/v3/activity/events/types/#organizationevent>.
data OrganizationEvent = OrganizationEvent
    { evOrganizationAction      :: !OrganizationEventAction
    , evOrganizationInvitation  :: !HookOrganizationInvitation
    , evOrganizationMembership  :: !HookOrganizationMembership
    , evOrganizationOrg         :: !HookOrganization
    , evOrganizationSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent OrganizationEvent where senderOfEvent = evOrganizationSender
instance NFData OrganizationEvent where rnf = genericRnf


type OrgBlockEventAction = Text

orgBlocksUser :: OrgBlockEventAction
orgBlocksUser = pack "blocked"

orgUnblocksUser :: OrgBlockEventAction
orgUnblocksUser = pack "unblocked"

-- | Triggered when an organization blocks or unblocks a user.
-- See <https://developer.github.com/v3/activity/events/types/#orgblockevent>.
data OrgBlockEvent = OrgBlockEvent
    { evOrgBlockAction          :: !OrgBlockEventAction
    , evOrgBlockUser            :: !HookUser
    , evOrgBlockOrg             :: !HookOrganization
    , evOrgBlockSender          :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent OrgBlockEvent where senderOfEvent = evOrgBlockSender
instance NFData OrgBlockEvent where rnf = genericRnf


-- | Represents an attempted build of a GitHub Pages site, whether successful or not.
-- Triggered on push to a GitHub Pages enabled branch (gh-pages for project pages, master for user and organization pages).
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#pagebuildevent>.
data PageBuildEvent = PageBuildEvent
    { evPageBuildId             :: !Int
    , evPageBuildResult         :: !HookPageBuildResult
    , evPageBuildRepo           :: !HookRepository
    , evPageBuildSender         :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PageBuildEvent where senderOfEvent = evPageBuildSender
instance NFData PageBuildEvent where rnf = genericRnf


type ProjectCardEventAction = Text

projectCardCreated :: ProjectCardEventAction
projectCardCreated = pack "created"

projectCardEdited :: ProjectCardEventAction
projectCardEdited = pack "edited"

projectCardConverted :: ProjectCardEventAction
projectCardConverted = pack "converted"

projectCardMoved :: ProjectCardEventAction
projectCardMoved = pack "moved"

projectCardDeleted :: ProjectCardEventAction
projectCardDeleted = pack "deleted"

-- | Triggered when a project card is created, updated, moved, converted to an issue, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectcardevent>.
data ProjectCardEvent = ProjectCardEvent
    { evProjectCardAction       :: !ProjectCardEventAction
    , evProjectCardPayload      :: !HookProjectCard
    , evProjectCardRepo         :: !HookRepository
    , evProjectCardOrg          :: !HookOrganization
    , evProjectCardSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent ProjectCardEvent where senderOfEvent = evProjectCardSender
instance NFData ProjectCardEvent where rnf = genericRnf


type ProjectColumnEventAction = Text

projectColumnCreated :: ProjectColumnEventAction
projectColumnCreated = pack "created"

projectColumnEdited :: ProjectColumnEventAction
projectColumnEdited = pack "edited"

projectColumnMoved :: ProjectColumnEventAction
projectColumnMoved = pack "moved"

projectColumnDeleted :: ProjectColumnEventAction
projectColumnDeleted = pack "deleted"

-- | Triggered when a project column is created, updated, moved, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectcolumnevent>.
data ProjectColumnEvent = ProjectColumnEvent
    { evProjectColumnAction     :: !ProjectColumnEventAction
    , evProjectColumnPayload    :: !HookProjectColumn
    , evProjectColumnRepo       :: !HookRepository
    , evProjectColumnOrg        :: !HookOrganization
    , evProjectColumnSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent ProjectColumnEvent where senderOfEvent = evProjectColumnSender
instance NFData ProjectColumnEvent where rnf = genericRnf


type ProjectEventAction = Text

projectCreated :: ProjectEventAction
projectCreated = pack "created"

projectEdited :: ProjectEventAction
projectEdited = pack "edited"

projectClosed :: ProjectEventAction
projectClosed = pack "closed"

projectReopened :: ProjectEventAction
projectReopened = pack "reopened"

projectDeleted :: ProjectEventAction
projectDeleted = pack "deleted"

-- | Triggered when a project is created, updated, closed, reopened, or deleted.
-- See <https://developer.github.com/v3/activity/events/types/#projectevent>.
data ProjectEvent = ProjectEvent
    { evProjectEventAction      :: !ProjectEventAction
    , evProjectPayload          :: !HookProject
    , evProjectRepo             :: !HookRepository
    , evProjectOrganization     :: !HookOrganization
    , evProjectSender           :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent ProjectEvent where senderOfEvent = evProjectSender
instance NFData ProjectEvent where rnf = genericRnf


-- | Triggered when a private repository is open sourced. Without a doubt: the best GitHub event.
-- See <https://developer.github.com/v3/activity/events/types/#publicevent>.
data PublicEvent = PublicEvent
    { evPublicEventRepo         :: !HookRepository
    , evPublicEventSender       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PublicEvent where senderOfEvent = evPublicEventSender
instance NFData PublicEvent where rnf = genericRnf


type PullRequestEventAction = Text

pullRequestAssigned :: PullRequestEventAction
pullRequestAssigned = pack "assigned"

pullRequestUnassigned :: PullRequestEventAction
pullRequestUnassigned = pack "unassigned"

pullRequestReviewRequested :: PullRequestEventAction
pullRequestReviewRequested = pack "review_requsted"

pullRequestReviewRequestRemoved :: PullRequestEventAction
pullRequestReviewRequestRemoved = pack "review_request_removed"

pullRequestLabeled :: PullRequestEventAction
pullRequestLabeled = pack "labeled"

pullRequestUnlabeled :: PullRequestEventAction
pullRequestUnlabeled = pack "unlabeled"

pullRequestOpened :: PullRequestEventAction
pullRequestOpened = pack "opened"

pullRequestEdited :: PullRequestEventAction
pullRequestEdited = pack "edited"

pullRequestClosed :: PullRequestEventAction
pullRequestClosed = pack "closed"

pullRequestReopened :: PullRequestEventAction
pullRequestReopened = pack "reopened"

-- | Triggered when a pull request is assigned, unassigned, labeled, unlabeled, opened, edited,
-- closed, reopened, or synchronized. Also triggered when a pull request review is requested,
-- or when a review request is removed.
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestevent>.
data PullRequestEvent = PullRequestEvent
    { evPullReqAction           :: !PullRequestEventAction
    , evPullReqNumber           :: !Int
    , evPullReqPayload          :: !HookPullRequest
    , evPullReqRepo             :: !HookRepository
    , evPullReqSender           :: !HookUser
    , evPullReqInstallationId   :: !Int
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PullRequestEvent where senderOfEvent = evPullReqSender
instance NFData PullRequestEvent where rnf = genericRnf


type PullRequestReviewEventAction = Text

pullRequestReviewSubmitted :: PullRequestReviewEventAction
pullRequestReviewSubmitted = pack "submitted"

pullRequestReviewEdited :: PullRequestReviewEventAction
pullRequestReviewEdited = pack "edited"

pullRequestReviewDismissed :: PullRequestReviewEventAction
pullRequestReviewDismissed = pack "dismissed"

-- | Triggered when a pull request review is submitted into a non-pending state,
-- the body is edited, or the review is dismissed.
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestreviewevent>.
data PullRequestReviewEvent = PullRequestReviewEvent
    { evPullReqReviewAction     :: !PullRequestReviewEventAction
    , evPullReqReviewPayload    :: !HookPullRequestReview
    , evPullReqReviewTarget     :: !HookPullRequest
    , evPullReqReviewRepo       :: !HookRepository
    , evPullReqReviewSender     :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PullRequestReviewEvent where senderOfEvent = evPullReqReviewSender
instance NFData PullRequestReviewEvent where rnf = genericRnf


type PullRequestReviewCommentEventAction = Text

pullRequestReviewCommentCreated :: PullRequestReviewCommentEventAction
pullRequestReviewCommentCreated = pack "created"

pullRequestReviewCommentEdited :: PullRequestReviewCommentEventAction
pullRequestReviewCommentEdited = pack "edited"

pullRequestReviewCommentDeleted :: PullRequestReviewCommentEventAction
pullRequestReviewCommentDeleted = pack "deleted"

-- | Triggered when a comment on a pull request's unified diff is created,
-- edited, or deleted (in the Files Changed tab).
-- See <https://developer.github.com/v3/activity/events/types/#pullrequestreviewcommentevent>.
data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { evPullReqRevComAction     :: !PullRequestReviewCommentEventAction
    , evPullReqRevComment       :: !HookPullRequestReviewComment
    , evPullReqRevTarget        :: !HookPullRequest
    , evPullReqRevRepo          :: !HookRepository
    , evPullReqRevSender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PullRequestReviewCommentEvent where senderOfEvent = evPullReqRevSender
instance NFData PullRequestReviewCommentEvent where rnf = genericRnf


-- | Triggered on a push to a repository branch. Branch pushes and repository tag
-- pushes also trigger webhook push events.
-- See <https://developer.github.com/v3/activity/events/types/#pushevent>.
data PushEvent = PushEvent
    { evPushRef                 :: !Text
    , evPushHeadSha             :: !(Maybe Text)
    , evPushBeforeSha           :: !(Maybe Text)
    , evPushCreated             :: !Bool
    , evPushDeleted             :: !Bool
    , evPushForced              :: !Bool
    , evPushBaseRef             :: !(Maybe Text)
    , evPushCompareUrl          :: !URL
    , evPushCommits             :: !(Maybe (Vector HookCommit))
    , evPushHeadCommit          :: !(Maybe HookCommit)
    , evPushRepository          :: !HookRepository
    , evPushOrganization        :: !(Maybe HookOrganization)
    , evPushSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent PushEvent where senderOfEvent = evPushSender
instance NFData PushEvent where rnf = genericRnf


type ReleaseEventAction = Text

releasePublished :: ReleaseEventAction
releasePublished = pack "published"

-- | Triggered when a release is published.
-- See <https://developer.github.com/v3/activity/events/types/#releaseevent>.
data ReleaseEvent = ReleaseEvent
    { evReleaseEventAction      :: !ReleaseEventAction      -- ^ Currently only releasePublished.
    , evReleaseEventPayload     :: !HookRelease
    , evReleaseEventRepo        :: !HookRepository
    , evReleaseEventSender      :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent ReleaseEvent where senderOfEvent = evReleaseEventSender
instance NFData ReleaseEvent where rnf = genericRnf


type RepositoryEventAction = Text

repositoryCreated :: RepositoryEventAction
repositoryCreated = "created"

repositoryDeleted :: RepositoryEventAction
repositoryDeleted = "deleted"

repositoryArchived :: RepositoryEventAction
repositoryArchived = "archived"

repositoryUnarchived :: RepositoryEventAction
repositoryUnarchived = "unarchived"

repositoryPublicized :: RepositoryEventAction
repositoryPublicized = "publicized"

repositoryPrivatized :: RepositoryEventAction
repositoryPrivatized = "privatized"

-- | Triggered when a repository is created, archived, unarchived, made public, or made private.
-- Organization hooks are also triggered when a repository is deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#repositoryevent>.
data RepositoryEvent = RepositoryEvent
    { evRepositoryAction        :: !RepositoryEventAction
    , evRepositoryTarget        :: !HookRepository
    , evRepositoryOrg           :: !(Maybe HookOrganization)
    , evRepositorySender        :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent RepositoryEvent where senderOfEvent = evRepositorySender
instance NFData RepositoryEvent where rnf = genericRnf


type StatusEventState = Text

gitCommitPending :: StatusEventState
gitCommitPending = pack "pending"

gitCommitSuccess :: StatusEventState
gitCommitSuccess = pack "success"

gitCommitFailure :: StatusEventState
gitCommitFailure = pack "failure"

gitCommitError :: StatusEventState
gitCommitError = pack "error"

-- | Triggered when the status of a Git commit changes.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#statusevent>.
data StatusEvent = StatusEvent
    { evStatusId                :: !Int
    , evStatusCommitSha         :: !Text
    , evStatusCommitName        :: !Text
    , evStatusTargetUrl         :: !(Maybe URL)
    , evStatusContext           :: !Text
    , evStatusDescription       :: !(Maybe Text)
    , evStatusState             :: !StatusEventState
    , evStatusCommit            :: !HookCommit
    -- FIXME: Branches are missing here
    , evStatusCreatedAt         :: !UTCTime
    , evStatusUpdatedAt         :: !UTCTime
    , evStatusRepo              :: !HookRepository
    , evStatusSender            :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent StatusEvent where senderOfEvent = evStatusSender
instance NFData StatusEvent where rnf = genericRnf


type TeamEventAction = Text

teamCreated :: TeamEventAction
teamCreated = pack "created"

teamDeleted :: TeamEventAction
teamDeleted = pack "deleted"

teamEdited :: TeamEventAction
teamEdited = pack "edited"

teamAddedToRepo :: TeamEventAction
teamAddedToRepo = pack "added_to_repository"

teamRemovedFromRepo :: TeamEventAction
teamRemovedFromRepo = pack "removed_from_repository"

-- | Triggered when an organization's team is created or deleted.
-- Events of this type are not visible in timelines. These events are only used to trigger organization hooks.
-- See <https://developer.github.com/v3/activity/events/types/#teamevent>.
data TeamEvent = TeamEvent
    { evTeamAction              :: !TeamEventAction
    , evTeamTarget              :: !HookTeam
    , evTeamOrganization        :: !HookOrganization
    , evTeamSender              :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent TeamEvent where senderOfEvent = evTeamSender
instance NFData TeamEvent where rnf = genericRnf


-- | Triggered when a repository is added to a team.
-- Events of this type are not visible in timelines. These events are only used to trigger hooks.
-- See <https://developer.github.com/v3/activity/events/types/#teamaddevent>.
data TeamAddEvent = TeamAddEvent
    { evTeamAddTarget           :: !(Maybe HookTeam) -- ^ Older events may not include this in the payload.
    , evTeamAddRepo             :: !HookRepository
    , evTeamAddOrg              :: !HookOrganization
    , evTeamAddSender           :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent TeamAddEvent where senderOfEvent = evTeamAddSender
instance NFData TeamAddEvent where rnf = genericRnf


type WatchEventAction = Text

startedWatching :: WatchEventAction
startedWatching = "started"

-- | The WatchEvent is related to starring a repository, not watching.
-- The event’s actor is the user who starred a repository, and the event’s
-- repository is the repository that was starred.
-- See <https://developer.github.com/v3/activity/events/types/#watchevent>.
data WatchEvent = WatchEvent
    { evWatchAction             :: !WatchEventAction -- ^ Currently can only be startedWatching
    , evWatchRepo               :: !HookRepository
    , evWatchSender             :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance WebHookEvent WatchEvent where senderOfEvent = evWatchSender
instance NFData WatchEvent where rnf = genericRnf


-- Aeson Instances

instance FromJSON CommitCommentEvent where
    parseJSON = withObject "CommitCommentEvent" $ \o -> CommitCommentEvent
        <$> o .: "action"
        <*> o .: "comment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON CreateEvent where
    parseJSON = withObject "CreateEvent" $ \o -> CreateEvent
        <$> o .: "ref"
        <*> o .: "ref_type"
        <*> o .: "master_branch"
        <*> o .: "description"
        <*> o .: "pusher_type"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeleteEvent where
    parseJSON = withObject "DeleteEvent" $ \o -> DeleteEvent
        <$> o .: "ref"
        <*> o .: "ref_type"
        <*> o .: "pusher_type"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeploymentEvent where
    parseJSON = withObject "DeploymentEvent" $ \o -> DeploymentEvent
        <$> o .: "deployment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON DeploymentStatusEvent where
    parseJSON = withObject "DeploymentStatusEvent" $ \o -> DeploymentStatusEvent
        <$> o .: "deployment_status"
        <*> o .: "deployment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON ForkEvent where
    parseJSON = withObject "ForkEvent" $ \o -> ForkEvent
        <$> o .: "forkee"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON GollumEvent where
    parseJSON = withObject "GollumEvent" $ \o -> GollumEvent
        <$> o .: "pages"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON InstallationEvent where
    parseJSON = withObject "InstallationEvent" $ \o -> InstallationEvent
        <$> o .: "action"
        <*> o .: "installation"
        <*> o .: "sender"

instance FromJSON InstallationRepositoriesEvent where
    parseJSON = withObject "InstallationRepositoriesEvent" $ \o -> InstallationRepositoriesEvent
        <$> o .: "action"
        <*> o .: "installation"
        <*> o .: "repository_selection"
        <*> o .: "repositories_added"
        <*> o .: "repositories_removed"
        <*> o .: "sender"

instance FromJSON IssueCommentEvent where
    parseJSON = withObject "IssueCommentEvent" $ \o -> IssueCommentEvent
        <$> o .: "action"
        <*> o .: "issue"
        <*> o .: "comment"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON IssuesEvent where
    parseJSON = withObject "IssuesEvent" $ \o -> IssuesEvent
        <$> o .: "action"
        <*> o .: "issue"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON LabelEvent where
    parseJSON = withObject "LabelEvent" $ \o -> LabelEvent
        <$> o .: "action"
        <*> o .: "label"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"

instance FromJSON MemberEvent where
    parseJSON = withObject "MemberEvent" $ \o -> MemberEvent
        <$> o .: "action"
        <*> o .: "member"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON MembershipEvent where
    parseJSON = withObject "MembershipEvent" $ \o -> MembershipEvent
        <$> o .: "action"
        <*> o .: "scope"
        <*> o .: "member"
        <*> o .: "team"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON MilestoneEvent where
    parseJSON = withObject "MilestoneEvent" $ \o -> MilestoneEvent
        <$> o .: "action"
        <*> o .: "milestone"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON OrganizationEvent where
    parseJSON = withObject "OrganizationEvent" $ \o -> OrganizationEvent
        <$> o .: "action"
        <*> o .: "invitation"
        <*> o .: "membership"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON OrgBlockEvent where
    parseJSON = withObject "OrgBlockEvent" $ \o -> OrgBlockEvent
        <$> o .: "action"
        <*> o .: "blocked_user"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON PageBuildEvent where
    parseJSON = withObject "PageBuildEvent" $ \o -> PageBuildEvent
        <$> o .: "id"
        <*> o .: "build"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON ProjectCardEvent where
    parseJSON = withObject "ProjectCardEvent" $ \o -> ProjectCardEvent
        <$> o .: "action"
        <*> o .: "project_card"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON ProjectColumnEvent where
    parseJSON = withObject "ProjectColumnEvent" $ \o -> ProjectColumnEvent
        <$> o .: "action"
        <*> o .: "project_column"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON ProjectEvent where
    parseJSON = withObject "ProjectEvent" $ \o -> ProjectEvent
        <$> o .: "action"
        <*> o .: "project"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON PublicEvent where
    parseJSON = withObject "PublicEvent" $ \o -> PublicEvent
        <$> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestEvent where
    parseJSON = withObject "PullRequestEvent" $ \o -> PullRequestEvent
        <$> o .: "action"
        <*> o .: "number"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"
        <*> (o .: "installation" >>= \i -> i .: "id")

instance FromJSON PullRequestReviewEvent where
    parseJSON = withObject "PullRequestReviewEvent" $ \o -> PullRequestReviewEvent
        <$> o .: "action"
        <*> o .: "review"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON = withObject "PullRequestReviewCommentEvent" $ \o -> PullRequestReviewCommentEvent
        <$> o .: "action"
        <*> o .: "comment"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PushEvent where
    parseJSON = withObject "PushEvent" $ \o -> PushEvent
        <$> o .: "ref"
        <*> o .:? "after"
        <*> o .:? "before"
        <*> o .: "created"
        <*> o .: "deleted"
        <*> o .: "forced"
        <*> o .:? "base_ref"
        <*> o .: "compare"
        <*> o .:? "commits"
        <*> o .:? "head_commit"
        <*> o .: "repository"
        <*> o .:? "organization"
        <*> o .: "sender"

instance FromJSON ReleaseEvent where
    parseJSON = withObject "ReleaseEvent" $ \o -> ReleaseEvent
        <$> o .: "action"
        <*> o .: "release"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON RepositoryEvent where
    parseJSON = withObject "RepositoryEvent" $ \o -> RepositoryEvent
        <$> o .: "action"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON StatusEvent where
    parseJSON = withObject "StatusEvent" $ \o -> StatusEvent
        <$> o .: "id"
        <*> o .: "sha"
        <*> o .: "name"
        <*> o .:? "target_url"
        <*> o .: "context"
        <*> o .:? "description"
        <*> o .: "state"
        <*> o .: "commit"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON TeamEvent where
    parseJSON = withObject "TeamEvent" $ \o -> TeamEvent
        <$> o .: "action"
        <*> o .: "team"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON TeamAddEvent where
    parseJSON = withObject "TeamAddEvent" $ \o -> TeamAddEvent
        <$> o .:? "team"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

instance FromJSON WatchEvent where
    parseJSON = withObject "WatchEvent" $ \o -> WatchEvent
        <$> o .: "action"
        <*> o .: "repository"
        <*> o .: "sender"
