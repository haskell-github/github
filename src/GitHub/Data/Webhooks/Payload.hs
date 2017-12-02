-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Structures used in Webhook payloads. These differ significantly enough
-- in structure from the types used elsewhere that they need to be segregated.

module GitHub.Data.Webhooks.Payload
    ( HookIssue(..)
    , HookRepository(..)
    , HookRepositoryLabel(..)
    , HookUser(..)
    , HookOrganization(..)
    , HookOrganizationInvitation(..)
    , HookOrganizationMembership(..)
    , HookTeam(..)
    , HookMilestone(..)
    , HookMembership(..)
    , HookProject(..)
    , HookProjectCard(..)
    , HookProjectColumn(..)
    , HookIssueLabels(..)
    , HookCommit(..)
    , HookRelease(..)
    , HookPullRequest(..)
    , HookPullRequestReview(..)
    , HookInstallation(..)
    , HookDeployment(..)
    , HookDeploymentStatus(..)
    , HookWikiPage(..)
    , HookPageBuildResult(..)
    , HookIssueComment(..)
    , HookCommitComment(..)
    , HookPullRequestReviewComment(..)
    ) where

import GitHub.Internal.Prelude
import GitHub.Data.Definitions    (OwnerType)
import GitHub.Data.URL            (URL)


type IssueState = Text

data HookIssue = HookIssue
    { whIssueUrl                :: !URL
    , whIssueLabelsUrl          :: !URL
    , whIssueCommentsUrl        :: !URL
    , whIssueEventsUrl          :: !URL
    , whIssueHtmlUrl            :: !URL
    , whIssueId                 :: !Int
    , whIssueNumber             :: !Int
    , whIssueTitle              :: !Text
    , whIssueUser               :: !HookUser
    , whIssueLabels             :: !(Vector HookIssueLabels)
    , whIssueState              :: IssueState
    , whIssueIsLocked           :: !Bool
    , whIssueAssignee           :: !(Maybe HookUser)
    , whIssueMilestone          :: !(Maybe HookMilestone)
    , whIssueCommentCount       :: !Int
    , whIssueCreatedAt          :: !UTCTime
    , whIssueUpdatedAt          :: !UTCTime
    , whIssueClosedAt           :: !(Maybe UTCTime)
    , whIssueBody               :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssue where rnf = genericRnf


data HookRepository = HookRepository
    { whRepoId                  :: !Int
    , whRepoName                :: !Text
    , whRepoFullName            :: !Text
    , whRepoOwner               :: !HookUser
    , whRepoIsPrivate           :: !Bool
    , whRepoHtmlUrl             :: !URL
    , whRepoDescription         :: !Text
    , whRepoIsAFork             :: !Bool
    , whRepoUrl                 :: !URL
    , whRepoForksUrl            :: !URL
    , whRepoKeysUrl             :: !URL
    , whRepoCollaboratorsUrl    :: !URL
    , whRepoTeamsUrl            :: !URL
    , whRepoHooksUrl            :: !URL
    , whRepoIssueEventsUrl      :: !URL
    , whRepoEventsUrl           :: !URL
    , whRepoAssigneesUrl        :: !URL
    , whRepoBranchesUrl         :: !URL
    , whRepoTagsUrl             :: !URL
    , whRepoBlobsUrl            :: !URL
    , whRepoGitTagsUrl          :: !URL
    , whRepoGitRefsUrl          :: !URL
    , whRepoTreesUrl            :: !URL
    , whRepoStatusesUrl         :: !URL
    , whRepoLanguagesUrl        :: !URL
    , whRepoStargazersUrl       :: !URL
    , whRepoContributorsUrl     :: !URL
    , whRepoSubscribersUrl      :: !URL
    , whRepoSubscriptionUrl     :: !URL
    , whRepoCommitsUrl          :: !URL
    , whRepoGitCommitsUrl       :: !URL
    , whRepoCommentsUrl         :: !URL
    , whRepoIssueCommentsUrl    :: !URL
    , whRepoContentsUrl         :: !URL
    , whRepoCompareUrl          :: !URL
    , whRepoMergesUrl           :: !URL
    , whRepoArchiveUrl          :: !URL
    , whRepoDownloadsUrl        :: !URL
    , whRepoIssuesUrl           :: !URL
    , whRepoPullsUrl            :: !URL
    , whRepoMilestonesUrl       :: !URL
    , whRepoNotificationsUrl    :: !URL
    , whRepoLabelsUrl           :: !URL
    , whRepoReleasesUrl         :: !URL
    , whRepoCreatedAt           :: !UTCTime
    , whRepoUpdatedAt           :: !UTCTime
    , whRepoPushedAt            :: !UTCTime
    , whRepoGitUrl              :: !URL
    , whRepoSshUrl              :: !URL
    , whRepoCloneUrl            :: !URL
    , whRepoSvnUrl              :: !URL
    , whRepoHomepage            :: !(Maybe URL)
    , whRepoSize                :: !Int
    , whRepoStargazersCount     :: !Int
    , whRepoWatchersCount       :: !Int
    , whRepoLanguage            :: !(Maybe Text)
    , whRepoHasIssues           :: !Bool
    , whRepoHasDownloads        :: !Bool
    , whRepoHasWiki             :: !Bool
    , whRepoHasPages            :: !Bool
    , whRepoForkCount           :: !Int
    , whRepoMirrorUrl           :: !(Maybe URL)
    , whRepoOpenIssuesCount     :: !Int
    , whRepoDefaultBranchName   :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRepository where rnf = genericRnf


data HookRepositoryLabel = HookRepositoryLabel
    { whRepoLabelUrl            :: !URL
    , whRepoLabelName           :: !Text
    , whRepoLabelColor          :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRepositoryLabel where rnf = genericRnf


data HookUser = HookUser
    { whUserLogin               :: !Text
    , whUserId                  :: !Int
    , whUserAvatarUrl           :: !URL
    , whUserGravatarId          :: !URL
    , whUserUrl                 :: !URL
    , whUserHtmlUrl             :: !URL
    , whUserFollowersUrl        :: !URL
    , whUserFollowingUrl        :: !URL
    , whUserGistsUrl            :: !URL
    , whUserStarredUrl          :: !URL
    , whUserSubscriptionsUrl    :: !URL
    , whUserOrganizationsUrl    :: !URL
    , whUserReposUrl            :: !URL
    , whUserEventsUrl           :: !URL
    , whUserReceivedEventsUrl   :: !URL
    , whUserType                :: !OwnerType
    , whUserIsAdminOfSite       :: !Bool
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookUser where rnf = genericRnf


data HookOrganization = HookOrganization
    { whOrgLogin                :: !Text
    , whOrgId                   :: !Int
    , whOrgUrl                  :: !URL
    , whOrgReposUrl             :: !URL
    , whOrgEventsUrl            :: !URL
    , whOrgHooksUrl             :: !URL
    , whOrgIssuesUrl            :: !URL
    , whOrgMembersUrl           :: !URL
    , whOrgPublicMembersUrl     :: !URL
    , whOrgAvatarUrl            :: !URL
    , whOrgDescription          :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganization where rnf = genericRnf


data HookOrganizationInvitation = HookOrganizationInvitation
    { whOrgInvitationId         :: !Int
    , whOrgInvitationLogin      :: !Text
    , whOrgInvitationEmail      :: !(Maybe Text)
    , whOrgInvitationRole       :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganizationInvitation where rnf = genericRnf


data HookOrganizationMembership = HookOrganizationMembership
    { whOrgMembershipUrl        :: !URL
    , whOrgMembershipState      :: !Text
    , whOrgMembershipRole       :: !Text
    , whOrgMembershipOrgUrl     :: !URL
    , whOrgMembershipUser       :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookOrganizationMembership where rnf = genericRnf


data HookTeam = HookTeam
    { whTeamName                :: !Text
    , whTeamId                  :: !Int
    , whTeamSlug                :: !Text
    , whTeamPermission          :: !Text
    , whTeamUrl                 :: !URL
    , whTeamMembersUrl          :: !URL
    , whTeamRepositoriesUrl     :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookTeam where rnf = genericRnf


type MilestoneState = Text

data HookMilestone = HookMilestone
    { whMilestoneUrl            :: !URL
    , whMilestoneHtmlUrl        :: !URL
    , whMilestoneLabelsUrl      :: !URL
    , whMilestoneId             :: !Int
    , whMilestoneNumber         :: !Int
    , whMilestoneTitle          :: !Text
    , whMilestoneDescription    :: !(Maybe Text)
    , whMilestoneCreator        :: !HookUser
    , whMilestoneOpenIssues     :: !Int
    , whMilestoneClosedIssues   :: !Int
    , whMilestoneState          :: !MilestoneState
    , whMilestoneCreatedAt      :: !UTCTime
    , whMilestoneUpdatedAt      :: !UTCTime
    , whMilestoneDueOn          :: !(Maybe UTCTime)
    , whMilestoneClosedAt       :: !(Maybe UTCTime)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookMilestone where rnf = genericRnf


type MembershipState = Text
type MembershipRole = Text

data HookMembership = HookMembership
    { whMembershipUrl           :: !URL
    , whMembershipState         :: !MembershipState
    , whMembershipRole          :: !MembershipRole
    , whMembershipOrgUrl        :: !URL
    , whMembershipUser          :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookMembership where rnf = genericRnf


type ProjectState = Text

data HookProject = HookProject
    { whProjectOwnerUrl         :: !URL
    , whProjectUrl              :: !URL
    , whProjectColumnsUrl       :: !URL
    , whProjectId               :: !URL
    , whProjectName             :: !Text
    , whProjectBody             :: !Text
    , whProjectNumber           :: !Int
    , whProjectState            :: !ProjectState
    , whProjectCreator          :: !HookUser
    , whProjectCreatedAt        :: !UTCTime
    , whProjectUpdatedAt        :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProject where rnf = genericRnf


data HookProjectCard = HookProjectCard
    { whProjectCardUrl          :: !URL
    , whProjectCardColumnUrl    :: !URL
    , whProjectCardColumnId     :: !Int
    , whProjectCardId           :: !Int
    , whProjectCardNote         :: !(Maybe Text)
    , whProjectCardCreator      :: !HookUser
    , whProjectCardCreatedAt    :: !UTCTime
    , whProjectCardUpdatedAt    :: !UTCTime
    , whProjectCardContentUrl   :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProjectCard where rnf = genericRnf


data HookProjectColumn = HookProjectColumn
    { whProjectColumnUrl        :: !URL
    , whProjectColumnProjUrl    :: !URL
    , whProjectColumnCardsUrl   :: !URL
    , whProjectColumnId         :: !Int
    , whProjectColumnName       :: !Text
    , whProjectColumnCreatedAt  :: !UTCTime
    , whProjectColumnUpdatedAt  :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookProjectColumn where rnf = genericRnf


data HookIssueLabels = HookIssueLabels
    { whIssueLabelId            :: !(Maybe Int)   -- ^ Not always sent.
    , whIssueLabelUrl           :: !URL
    , whIssueLabelName          :: !Text
    , whIssueLabelColor         :: !Text
    , whIssueLabelIsDefault     :: !Bool          -- ^ Defaults to false when not present.
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssueLabels where rnf = genericRnf


-- FIXME: Missing nested metadata that provides commit description
-- FIXME: Missing property "parent" (no examples provided)
data HookCommit = HookCommit
    { whCommitSha               :: !Text
    , whCommitUrl               :: !URL
    , whCommitHtmlUrl           :: !URL
    , whCommitCommentsUrl       :: !URL
    , whCommitAuthor            :: !HookUser
    , whCommitCommitter         :: !HookUser
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCommit where rnf = genericRnf


-- FIXME: Missing property "assets" (no examples provided)
data HookRelease = HookRelease
    { whReleaseUrl              :: !URL
    , whReleaseAssetsUrl        :: !URL
    , whReleaseUploadUrl        :: !URL
    , whReleaseHtmlUrl          :: !URL
    , whReleaseId               :: !Int
    , whReleaseTagName          :: !Text
    , whReleaseTargetCommitish  :: !Text
    , whReleaseName             :: !(Maybe Text)
    , whReleaseIsDraft          :: !Bool
    , whReleaseAuthor           :: !HookUser
    , whReleaseIsPreRelease     :: !Bool
    , whReleaseCreatedAt        :: !UTCTime
    , whReleasePublishedAt      :: !(Maybe UTCTime)
    , whReleaseTarballUrl       :: !URL
    , whReleaseZipballUrl       :: !URL
    , whReleaseBody             :: !(Maybe Text)
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookRelease where rnf = genericRnf


-- FIXME: Property "head" missing (not sure)
-- FIXME: Property "base" missing (not sure)
data HookPullRequest = HookPullRequest
    { whPullReqUrl              :: !URL
    , whPullReqId               :: !Int
    , whPullReqHtmlUrl          :: !URL
    , whPullReqDiffUrl          :: !URL
    , whPullReqPatchUrl         :: !URL
    , whPullReqIssueUrl         :: !URL
    , whPullReqNumber           :: !Int
    , whPullReqState            :: !Text -- ^ FIXME: Smart constructor?
    , whPullReqIsLocked         :: !Bool
    , whPullReqTitle            :: !Text
    , whPullReqUser             :: !HookUser
    , whPullReqBody             :: !Text
    , whPullReqCreatedAt        :: !UTCTime
    , whPullReqUpdatedAt        :: !UTCTime
    , whPullReqClosedAt         :: !(Maybe UTCTime)
    , whPullReqMergedAt         :: !(Maybe UTCTime)
    , whPullReqMergeCommitSha   :: !(Maybe Text)
    , whPullReqAssignee         :: !(Maybe HookUser)
    , whPullReqMilestone        :: !(Maybe HookMilestone)
    , whPullReqCommitsUrl       :: !URL
    , whPullReqRevCommentsUrl   :: !URL
    , whPullReqRevCommentUrl    :: !URL
    , whPullReqCommentsUrl      :: !URL
    , whPullReqStatusesUrl      :: !URL
    , whPullReqIsMerged         :: !Bool
    -- , whPullReqIsMergeable      :: !Bool
    , whPullReqMergeableState   :: !Text
    , whPullReqMergedBy         :: !(Maybe HookUser)
    , whPullReqCommentCount     :: !Int
    , whPullReqRevCommentCount  :: !Int
    , whPullReqCommitCount      :: !Int
    , whPullReqAdditionsCount   :: !Int
    , whPullReqDeletionsCount   :: !Int
    , whPullReqFileChangeCount  :: !Int
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequest where rnf = genericRnf


data HookPullRequestReview = HookPullRequestReview
    { whPullReqReviewId         :: !Int
    , whPullReqReviewUser       :: !HookUser
    , whPullReqReviewBody       :: !Text
    , whPullReqReviewSubmittedAt :: !UTCTime
    , whPullReqReviewState      :: !Text
    , whPullReqReviewHtmlUrl    :: !URL
    , whPullReqReviewPullUrl    :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequestReview where rnf = genericRnf


data HookInstallation = HookInstallation
    { whInstallationId          :: !Int
    , whInstallationAccount     :: !HookUser
    , whInstallationRepoSel     :: !Text
    , whInstallationTokenUrl    :: !URL
    , whInstallationRepoUrl     :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookInstallation where rnf = genericRnf


data HookDeployment = HookDeployment
    { whDeploymentUrl           :: !URL
    , whDeploymentId            :: !Int
    , whDeploymentSha           :: !Text
    , whDeploymentRef           :: !Text
    , whDeploymentTask          :: !Text
    -- , whDeploymentPayload
    , whDeploymentEnv           :: !Text
    , whDeploymentDescription   :: !(Maybe Text)
    , whDeploymentCreator       :: !HookUser
    , whDeploymentCreatedAt     :: !UTCTime
    , whDeploymentUpdatedAt     :: !UTCTime
    , whDeploymentStatusesUrl   :: !URL
    , whDeploymentRepoUrl       :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookDeployment where rnf = genericRnf


data HookDeploymentStatus = HookDeploymentStatus
    { whDeploymentStatusUrl     :: !URL
    , whDeploymentStatusId      :: !Int
    , whDeploymentStatusState   :: !Text
    , whDeploymentStatusCreator :: !HookUser
    , whDeploymentStatusDesc    :: !(Maybe Text)
    , whDeploymentStatusTargetUrl :: !(Maybe URL)
    , whDeploymentStatusCreatedAt :: !UTCTime
    , whDeploymentStatusUpdatedAt :: !UTCTime
    , whDeploymentStatusDeplUrl   :: !URL
    , whDeploymentStatusRepoUrl   :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookDeploymentStatus where rnf = genericRnf


data HookWikiPage = HookWikiPage
    { whWikiPageName            :: !Text
    , whWikiPageTitle           :: !Text
    , whWikiPageSummary         :: !(Maybe Text)
    , wkWikiPageAction          :: !Text
    , whWikiPageSha             :: !Text
    , whWikiPageHtmlUrl         :: URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookWikiPage where rnf = genericRnf


data HookPageBuildResult = HookPageBuildResult
    { whPageBuildUrl            :: !URL
    , whPageBuildStatus         :: !Text
    , whPageBuildError          :: !(Maybe Text)
    , whPageBuildPusher         :: !HookUser
    , whPageBuildCommitSha      :: !Text
    , whPageBuildDuration       :: !Int
    , whPageBuildCreatedAt      :: !UTCTime
    , whPageBuildUpdatedAt      :: !UTCTime
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPageBuildResult where rnf = genericRnf


data HookIssueComment = HookIssueComment
    { whIssueCommentUrl         :: !URL
    , whIssueCommentHtmlUrl     :: !URL
    , whIssueCommentIssueUrl    :: !URL
    , whIssueCommentId          :: !Int
    , whIssueCommentUser        :: !HookUser
    , whIssueCommentCreatedAt   :: !UTCTime
    , whIssueCommentUpdatedAt   :: !UTCTime
    , whIssueCommentBody        :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookIssueComment where rnf = genericRnf


data HookCommitComment = HookCommitComment
    { whCommitCommentUrl        :: !URL
    , whCommitCommentHtmlUrl    :: !URL
    , whCommitCommentId         :: !Int
    , whCommitCommentUser       :: !HookUser
    , whCommitCommentPos        :: !(Maybe Int)
    , whCommitCommentLine       :: !(Maybe Int)
    , whCommitCommentPath       :: !(Maybe Text)
    , whCommitCommentCommitSha  :: !Text
    , whCommitCommentCreatedAt  :: !UTCTime
    , whCommitCommentUpdatedAt  :: !UTCTime
    , whCommitCommentBody       :: !Text
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookCommitComment where rnf = genericRnf


data HookPullRequestReviewComment = HookPullRequestReviewComment
    { whPullReqRevComUrl        :: !URL
    , whPullReqRevComId         :: !Int
    , whPullReqRevComDiffHunk   :: !Text
    , whPullReqRevComPath       :: !Text
    , whPullReqRevComPos        :: !Int
    , whPullReqRevComOrigPos    :: !Int
    , whPullReqRevComCommitSha  :: !Text
    , whPullReqRevComOrigSha    :: !Text
    , whPullReqRevComUser       :: !HookUser
    , whPullReqRevComBody       :: !Text
    , whPullReqRevComCreatedAt  :: !UTCTime
    , whPullReqRevComUpdatedAt  :: !UTCTime
    , whPullReqRevComHtmlUrl    :: !URL
    , whPullReqRevComPullReqUrl :: !URL
    }
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData HookPullRequestReviewComment where rnf = genericRnf


-- Aeson Instances

instance FromJSON HookIssue where
  parseJSON = withObject "HookIssue" $ \o -> HookIssue
      <$> o .: "url"
      <*> o .: "labels_url"
      <*> o .: "comments_url"
      <*> o .: "events_url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "number"
      <*> o .: "title"
      <*> o .: "user"
      <*> o .: "labels"
      <*> o .: "state"
      <*> o .: "locked"
      <*> o .: "assignee"
      <*> o .: "milestone"
      <*> o .: "comments"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "closed_at"
      <*> o .: "body"

instance FromJSON HookRepository where
  parseJSON = withObject "HookRepository" $ \o -> HookRepository
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "full_name"
      <*> o .: "owner"
      <*> o .: "private"
      <*> o .: "html_url"
      <*> o .: "description"
      <*> o .: "fork"
      <*> o .: "url"
      <*> o .: "forks_url"
      <*> o .: "keys_url"
      <*> o .: "collaborators_url"
      <*> o .: "teams_url"
      <*> o .: "hooks_url"
      <*> o .: "issue_events_url"
      <*> o .: "events_url"
      <*> o .: "assignees_url"
      <*> o .: "branches_url"
      <*> o .: "tags_url"
      <*> o .: "blobs_url"
      <*> o .: "git_tags_url"
      <*> o .: "git_refs_url"
      <*> o .: "trees_url"
      <*> o .: "statuses_url"
      <*> o .: "languages_url"
      <*> o .: "stargazers_url"
      <*> o .: "contributors_url"
      <*> o .: "subscribers_url"
      <*> o .: "subscription_url"
      <*> o .: "commits_url"
      <*> o .: "git_commits_url"
      <*> o .: "comments_url"
      <*> o .: "issue_comment_url"
      <*> o .: "contents_url"
      <*> o .: "compare_url"
      <*> o .: "merges_url"
      <*> o .: "archive_url"
      <*> o .: "downloads_url"
      <*> o .: "issues_url"
      <*> o .: "pulls_url"
      <*> o .: "milestones_url"
      <*> o .: "notifications_url"
      <*> o .: "labels_url"
      <*> o .: "releases_url"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "pushed_at"
      <*> o .: "git_url"
      <*> o .: "ssh_url"
      <*> o .: "clone_url"
      <*> o .: "svn_url"
      <*> o .:? "homepage"
      <*> o .: "size"
      <*> o .: "stargazers_count"
      <*> o .: "watchers_count"
      <*> o .: "language"
      <*> o .: "has_issues"
      <*> o .: "has_downloads"
      <*> o .: "has_wiki"
      <*> o .: "has_pages"
      <*> o .: "forks_count"
      <*> o .:? "mirror_url"
      <*> o .: "open_issues_count"
      <*> o .: "default_branch"

instance FromJSON HookRepositoryLabel where
  parseJSON = withObject "HookRepositoryLabel" $ \o -> HookRepositoryLabel
      <$> o .: "url"
      <*> o .: "name"
      <*> o .: "color"

instance FromJSON HookUser where
  parseJSON = withObject "HookUser" $ \o -> HookUser
      <$> o .: "login"
      <*> o .: "id"
      <*> o .: "avatar_url"
      <*> o .: "gravatar_id"
      <*> o .: "url"
      <*> o .: "html_url"
      <*> o .: "followers_url"
      <*> o .: "following_url"
      <*> o .: "gists_url"
      <*> o .: "starred_url"
      <*> o .: "subscriptions_url"
      <*> o .: "organizations_url"
      <*> o .: "repos_url"
      <*> o .: "events_url"
      <*> o .: "received_events_url"
      <*> o .: "type"
      <*> o .: "site_admin"

instance FromJSON HookOrganization where
  parseJSON = withObject "HookOrganization" $ \o -> HookOrganization
      <$> o .: "login"
      <*> o .: "id"
      <*> o .: "url"
      <*> o .: "repos_url"
      <*> o .: "events_url"
      <*> o .: "hooks_url"
      <*> o .: "issues_url"
      <*> o .: "members_url"
      <*> o .: "public_members_url"
      <*> o .: "avatar_url"
      <*> o .: "description"

instance FromJSON HookOrganizationInvitation where
  parseJSON = withObject "HookOrganizationInvitation" $ \o -> HookOrganizationInvitation
      <$> o .: "id"
      <*> o .: "login"
      <*> o .: "email"
      <*> o .: "role"

instance FromJSON HookOrganizationMembership where
  parseJSON = withObject "HookOrganizationMembership" $ \o -> HookOrganizationMembership
      <$> o .: "url"
      <*> o .: "state"
      <*> o .: "role"
      <*> o .: "organization_url"
      <*> o .: "user"

instance FromJSON HookTeam where
  parseJSON = withObject "HookTeam" $ \o -> HookTeam
      <$> o .: "name"
      <*> o .: "id"
      <*> o .: "slug"
      <*> o .: "permission"
      <*> o .: "url"
      <*> o .: "members_url"
      <*> o .: "repositories_url"

instance FromJSON HookMilestone where
  parseJSON = withObject "HookMilestone" $ \o -> HookMilestone
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "labels_url"
      <*> o .: "id"
      <*> o .: "number"
      <*> o .: "title"
      <*> o .: "description"
      <*> o .: "creator"
      <*> o .: "open_issues"
      <*> o .: "closed_issues"
      <*> o .: "state"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "due_on"
      <*> o .: "closed_at"

instance FromJSON HookMembership where
  parseJSON = withObject "HookMembership" $ \o -> HookMembership
      <$> o .: "url"
      <*> o .: "state"
      <*> o .: "role"
      <*> o .: "organization_url"
      <*> o .: "user"

instance FromJSON HookProject where
  parseJSON = withObject "HookProject" $ \o -> HookProject
      <$> o .: "owner_url"
      <*> o .: "url"
      <*> o .: "columns_url"
      <*> o .: "id"
      <*> o .: "name"
      <*> o .: "body"
      <*> o .: "number"
      <*> o .: "state"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookProjectCard where
  parseJSON = withObject "HookProjectCard" $ \o -> HookProjectCard
      <$> o .: "url"
      <*> o .: "column_url"
      <*> o .: "column_id"
      <*> o .: "id"
      <*> o .:? "note"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "content_url"

instance FromJSON HookProjectColumn where
  parseJSON = withObject "HookProjectColumn" $ \o -> HookProjectColumn
      <$> o .: "url"
      <*> o .: "project_url"
      <*> o .: "cards_url"
      <*> o .: "id"
      <*> o .: "name"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookIssueLabels where
  parseJSON = withObject "HookIssueLabels" $ \o -> HookIssueLabels
      <$> o .:? "id"
      <*> o .: "url"
      <*> o .: "name"
      <*> o .: "color"
      <*> o .:? "default" .!= False

instance FromJSON HookCommit where
  parseJSON = withObject "HookCommit" $ \o -> HookCommit
      <$> o .: "sha"
      <*> o .: "url"
      <*> o .: "html_url"
      <*> o .: "comments_url"
      <*> o .: "author"
      <*> o .: "committer"

instance FromJSON HookRelease where
  parseJSON = withObject "HookRelease" $ \o -> HookRelease
      <$> o .: "url"
      <*> o .: "assets_url"
      <*> o .: "upload_url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "tag_name"
      <*> o .: "target_commitish"
      <*> o .:? "name"
      <*> o .: "draft"
      <*> o .: "author"
      <*> o .: "prerelease"
      <*> o .: "created_at"
      <*> o .:? "published_at"
      <*> o .: "tarball_url"
      <*> o .: "zipball_url"
      <*> o .:? "body"

instance FromJSON HookPullRequest where
  parseJSON = withObject "HookPullRequest" $ \o -> HookPullRequest
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "html_url"
      <*> o .: "diff_url"
      <*> o .: "patch_url"
      <*> o .: "issue_url"
      <*> o .: "number"
      <*> o .: "state"
      <*> o .: "locked"
      <*> o .: "title"
      <*> o .: "user"
      <*> o .: "body"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "closed_at"
      <*> o .:? "merged_at"
      <*> o .:? "merge_commit_sha"
      <*> o .:? "assignee"
      <*> o .:? "milestone"
      <*> o .: "commits_url"
      <*> o .: "review_comments_url"
      <*> o .: "review_comment_url"
      <*> o .: "comments_url"
      <*> o .: "statuses_url"
      <*> o .: "merged"
      -- <*> o .: "mergeable"
      <*> o .: "mergeable_state"
      <*> o .: "merged_by"
      <*> o .: "comments"
      <*> o .: "review_comments"
      <*> o .: "commits"
      <*> o .: "additions"
      <*> o .: "deletions"
      <*> o .: "changed_files"

instance FromJSON HookPullRequestReview where
  parseJSON = withObject "HookPullRequestReview" $ \o -> HookPullRequestReview
      <$> o .: "id"
      <*> o .: "user"
      <*> o .: "body"
      <*> o .: "submitted_at"
      <*> o .: "state"
      <*> o .: "html_url"
      <*> o .: "pull_request_url"

instance FromJSON HookInstallation where
  parseJSON = withObject "HookInstallation" $ \o -> HookInstallation
      <$> o .: "id"
      <*> o .: "account"
      <*> o .: "repository_selection"
      <*> o .: "access_tokens_url"
      <*> o .: "repositories_url"

instance FromJSON HookDeployment where
  parseJSON = withObject "HookDeployment" $ \o -> HookDeployment
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "sha"
      <*> o .: "ref"
      <*> o .: "task"
      <*> o .: "environment"
      <*> o .:? "description"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "statuses_url"
      <*> o .: "repository_url"

instance FromJSON HookDeploymentStatus where
  parseJSON = withObject "HookDeploymentStatus" $ \o -> HookDeploymentStatus
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "state"
      <*> o .: "creator"
      <*> o .:? "description"
      <*> o .:? "target_url"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "deployment_url"
      <*> o .: "repository_url"

instance FromJSON HookWikiPage where
  parseJSON = withObject "HookWikiPage" $ \o -> HookWikiPage
      <$> o .: "page_name"
      <*> o .: "title"
      <*> o .:? "summary"
      <*> o .: "action"
      <*> o .: "sha"
      <*> o .: "html_url"

instance FromJSON HookPageBuildResult where
  parseJSON = withObject "HookPageBuildResult" $ \o -> HookPageBuildResult
      <$> o .: "url"
      <*> o .: "status"
      <*> (o .: "error" >>= \e -> e .:? "message")
      <*> o .: "pusher"
      <*> o .: "commit"
      <*> o .: "duration"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON HookIssueComment where
  parseJSON = withObject "HookIssueComment" $ \o -> HookIssueComment
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "issue_url"
      <*> o .: "id"
      <*> o .: "user"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "body"

instance FromJSON HookCommitComment where
  parseJSON = withObject "HookCommitComment" $ \o -> HookCommitComment
      <$> o .: "url"
      <*> o .: "html_url"
      <*> o .: "id"
      <*> o .: "user"
      <*> o .:? "position"
      <*> o .:? "line"
      <*> o .:? "path"
      <*> o .: "commit_id"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "body"

instance FromJSON HookPullRequestReviewComment where
  parseJSON = withObject "HookPullRequestReviewComment" $ \o -> HookPullRequestReviewComment
      <$> o .: "url"
      <*> o .: "id"
      <*> o .: "diff_hunk"
      <*> o .: "path"
      <*> o .: "position"
      <*> o .: "original_position"
      <*> o .: "commit_id"
      <*> o .: "original_commit_id"
      <*> o .: "user"
      <*> o .: "body"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "html_url"
      <*> o .: "pull_request_url"
