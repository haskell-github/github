{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Github.Data.Webhooks where

import Github.Data.Definitions
import Github.Data.Id

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import GHC.Generics    (Generic)

import qualified Data.Map as M

data RepoWebhook = RepoWebhook {
   repoWebhookUrl :: String
  ,repoWebhookTestUrl :: String
  ,repoWebhookId :: Id RepoWebhook
  ,repoWebhookName :: String
  ,repoWebhookActive :: Bool
  ,repoWebhookEvents :: [RepoWebhookEvent]
  ,repoWebhookConfig :: M.Map String String
  ,repoWebhookLastResponse :: RepoWebhookResponse
  ,repoWebhookUpdatedAt :: GithubDate
  ,repoWebhookCreatedAt :: GithubDate
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhook

data RepoWebhookEvent =
   WebhookWildcardEvent
 | WebhookCommitCommentEvent
 | WebhookCreateEvent
 | WebhookDeleteEvent
 | WebhookDeploymentEvent
 | WebhookDeploymentStatusEvent
 | WebhookForkEvent
 | WebhookGollumEvent
 | WebhookIssueCommentEvent
 | WebhookIssuesEvent
 | WebhookMemberEvent
 | WebhookPageBuildEvent
 | WebhookPublicEvent
 | WebhookPullRequestReviewCommentEvent
 | WebhookPullRequestEvent
 | WebhookPushEvent
 | WebhookReleaseEvent
 | WebhookStatusEvent
 | WebhookTeamAddEvent
 | WebhookWatchEvent
   deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookEvent

data RepoWebhookResponse = RepoWebhookResponse {
   repoWebhookResponseCode :: Maybe Int
  ,repoWebhookResponseStatus :: String
  ,repoWebhookResponseMessage :: Maybe String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookResponse

data PingEvent = PingEvent {
   pingEventZen :: String
  ,pingEventHook :: RepoWebhook
  ,pingEventHookId :: Id RepoWebhook
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PingEvent

data NewRepoWebhook = NewRepoWebhook {
  newRepoWebhookName   :: String
 ,newRepoWebhookConfig :: M.Map String String
 ,newRepoWebhookEvents :: Maybe [RepoWebhookEvent]
 ,newRepoWebhookActive :: Maybe Bool
} deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData NewRepoWebhook

data EditRepoWebhook = EditRepoWebhook {
  editRepoWebhookConfig       :: Maybe (M.Map String String)
 ,editRepoWebhookEvents       :: Maybe [RepoWebhookEvent]
 ,editRepoWebhookAddEvents    :: Maybe [RepoWebhookEvent]
 ,editRepoWebhookRemoveEvents :: Maybe [RepoWebhookEvent]
 ,editRepoWebhookActive       :: Maybe Bool
} deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData EditRepoWebhook
