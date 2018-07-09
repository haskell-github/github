-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Webhooks where

import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Map as M

data RepoWebhook = RepoWebhook
    { repoWebhookUrl          :: !URL
    , repoWebhookTestUrl      :: !URL
    , repoWebhookId           :: !(Id RepoWebhook)
    , repoWebhookName         :: !Text
    , repoWebhookActive       :: !Bool
    , repoWebhookEvents       :: !(Vector RepoWebhookEvent)
    , repoWebhookConfig       :: !(M.Map Text Text)
    , repoWebhookLastResponse :: !RepoWebhookResponse
    , repoWebhookUpdatedAt    :: !UTCTime
    , repoWebhookCreatedAt    :: !UTCTime
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhook where rnf = genericRnf
instance Binary RepoWebhook

data RepoWebhookEvent
    = WebhookWildcardEvent
    | WebhookCommitCommentEvent
    | WebhookCreateEvent
    | WebhookDeleteEvent
    | WebhookDeploymentEvent
    | WebhookDeploymentStatusEvent
    | WebhookForkEvent
    | WebhookGollumEvent
    | WebhookInstallationEvent
    | WebhookInstallationRepositoriesEvent
    | WebhookIssueCommentEvent
    | WebhookIssuesEvent
    | WebhookMemberEvent
    | WebhookPageBuildEvent
    | WebhookPingEvent
    | WebhookPublicEvent
    | WebhookPullRequestReviewCommentEvent
    | WebhookPullRequestEvent
    | WebhookPushEvent
    | WebhookReleaseEvent
    | WebhookStatusEvent
    | WebhookTeamAddEvent
    | WebhookWatchEvent
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookEvent where rnf = genericRnf
instance Binary RepoWebhookEvent

data RepoWebhookResponse = RepoWebhookResponse
    { repoWebhookResponseCode    :: !(Maybe Int)
    , repoWebhookResponseStatus  :: !Text
    , repoWebhookResponseMessage :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookResponse where rnf = genericRnf
instance Binary RepoWebhookResponse

data PingEvent = PingEvent
    { pingEventZen    :: !Text
    , pingEventHook   :: !RepoWebhook
    , pingEventHookId :: !(Id RepoWebhook)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PingEvent where rnf = genericRnf
instance Binary PingEvent

data NewRepoWebhook = NewRepoWebhook
    { newRepoWebhookName   :: !Text
    , newRepoWebhookConfig :: !(M.Map Text Text)
    , newRepoWebhookEvents :: !(Maybe (Vector RepoWebhookEvent))
    , newRepoWebhookActive :: !(Maybe Bool)
    }
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData NewRepoWebhook where rnf = genericRnf
instance Binary NewRepoWebhook

data EditRepoWebhook = EditRepoWebhook
    { editRepoWebhookConfig       :: !(Maybe (M.Map Text Text))
    , editRepoWebhookEvents       :: !(Maybe (Vector RepoWebhookEvent))
    , editRepoWebhookAddEvents    :: !(Maybe (Vector RepoWebhookEvent))
    , editRepoWebhookRemoveEvents :: !(Maybe (Vector RepoWebhookEvent))
    , editRepoWebhookActive       :: !(Maybe Bool)
    }
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData EditRepoWebhook where rnf = genericRnf
instance Binary EditRepoWebhook

-- JSON instances

instance FromJSON RepoWebhookEvent where
    parseJSON (String "*") = pure WebhookWildcardEvent
    parseJSON (String "commit_comment") = pure WebhookCommitCommentEvent
    parseJSON (String "create") = pure WebhookCreateEvent
    parseJSON (String "delete") = pure WebhookDeleteEvent
    parseJSON (String "deployment") = pure WebhookDeploymentEvent
    parseJSON (String "deployment_status") = pure WebhookDeploymentStatusEvent
    parseJSON (String "fork") = pure WebhookForkEvent
    parseJSON (String "gollum") = pure WebhookGollumEvent
    parseJSON (String "installation") = pure WebhookInstallationEvent
    parseJSON (String "installation_repositories") = pure WebhookInstallationRepositoriesEvent
    parseJSON (String "issue_comment") = pure WebhookIssueCommentEvent
    parseJSON (String "issues") = pure WebhookIssuesEvent
    parseJSON (String "member") = pure WebhookMemberEvent
    parseJSON (String "page_build") = pure WebhookPageBuildEvent
    parseJSON (String "ping") = pure WebhookPingEvent
    parseJSON (String "public") = pure WebhookPublicEvent
    parseJSON (String "pull_request_review_comment") = pure WebhookPullRequestReviewCommentEvent
    parseJSON (String "pull_request") = pure WebhookPullRequestEvent
    parseJSON (String "push") = pure WebhookPushEvent
    parseJSON (String "release") = pure WebhookReleaseEvent
    parseJSON (String "status") = pure WebhookStatusEvent
    parseJSON (String "team_add") = pure WebhookTeamAddEvent
    parseJSON (String "watch") = pure WebhookWatchEvent
    parseJSON _ = fail "Could not build a Webhook event"

instance ToJSON RepoWebhookEvent where
    toJSON WebhookWildcardEvent                 = String "*"
    toJSON WebhookCommitCommentEvent            = String "commit_comment"
    toJSON WebhookCreateEvent                   = String "create"
    toJSON WebhookDeleteEvent                   = String "delete"
    toJSON WebhookDeploymentEvent               = String "deployment"
    toJSON WebhookDeploymentStatusEvent         = String "deployment_status"
    toJSON WebhookForkEvent                     = String "fork"
    toJSON WebhookGollumEvent                   = String "gollum"
    toJSON WebhookInstallationEvent             = String "installation"
    toJSON WebhookInstallationRepositoriesEvent = String "installation_repositories"
    toJSON WebhookIssueCommentEvent             = String "issue_comment"
    toJSON WebhookIssuesEvent                   = String "issues"
    toJSON WebhookMemberEvent                   = String "member"
    toJSON WebhookPageBuildEvent                = String "page_build"
    toJSON WebhookPingEvent                     = String "ping"
    toJSON WebhookPublicEvent                   = String "public"
    toJSON WebhookPullRequestReviewCommentEvent = String "pull_request_review_comment"
    toJSON WebhookPullRequestEvent              = String "pull_request"
    toJSON WebhookPushEvent                     = String "push"
    toJSON WebhookReleaseEvent                  = String "release"
    toJSON WebhookStatusEvent                   = String "status"
    toJSON WebhookTeamAddEvent                  = String "team_add"
    toJSON WebhookWatchEvent                    = String "watch"

instance FromJSON RepoWebhook where
    parseJSON = withObject "RepoWebhook" $ \o -> RepoWebhook
        <$> o .: "url"
        <*> o .: "test_url"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "active"
        <*> o .: "events"
        <*> o .: "config"
        <*> o .: "last_response"
        <*> o .: "updated_at"
        <*> o .: "created_at"

instance FromJSON RepoWebhookResponse where
    parseJSON = withObject "RepoWebhookResponse" $ \o -> RepoWebhookResponse
        <$> o .: "code"
        <*> o .: "status"
        <*> o .: "message"

instance ToJSON NewRepoWebhook where
  toJSON (NewRepoWebhook { newRepoWebhookName = name
                         , newRepoWebhookConfig = config
                         , newRepoWebhookEvents = events
                         , newRepoWebhookActive = active

             }) = object
             [ "name" .= name
             , "config" .= config
             , "events" .= events
             , "active" .= active
             ]

instance ToJSON EditRepoWebhook where
  toJSON (EditRepoWebhook { editRepoWebhookConfig = config
                          , editRepoWebhookEvents = events
                          , editRepoWebhookAddEvents = addEvents
                          , editRepoWebhookRemoveEvents = removeEvents
                          , editRepoWebhookActive = active
             }) = object
             [ "config" .= config
             , "events" .= events
             , "add_events" .= addEvents
             , "remove_events" .= removeEvents
             , "active" .= active
             ]

instance FromJSON PingEvent where
    parseJSON = withObject "PingEvent" $ \o -> PingEvent
        <$> o .: "zen"
        <*> o .: "hook"
        <*> o .: "hook_id"
