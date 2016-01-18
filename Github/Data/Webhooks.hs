{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Github.Data.Webhooks where

import Github.Data.Id (Id)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

import qualified Data.Map as M

data RepoWebhook = RepoWebhook {
   repoWebhookUrl          :: !Text
  ,repoWebhookTestUrl      :: !Text
  ,repoWebhookId           :: !(Id RepoWebhook)
  ,repoWebhookName         :: !Text
  ,repoWebhookActive       :: !Bool
  ,repoWebhookEvents       :: !(Vector RepoWebhookEvent)
  ,repoWebhookConfig       :: !(M.Map Text Text)
  ,repoWebhookLastResponse :: !RepoWebhookResponse
  ,repoWebhookUpdatedAt    :: !UTCTime
  ,repoWebhookCreatedAt    :: !UTCTime
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhook where rnf = genericRnf
instance Binary RepoWebhook

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

instance NFData RepoWebhookEvent where rnf = genericRnf
instance Binary RepoWebhookEvent

data RepoWebhookResponse = RepoWebhookResponse {
   repoWebhookResponseCode    :: !(Maybe Int)
  ,repoWebhookResponseStatus  :: !Text
  ,repoWebhookResponseMessage :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoWebhookResponse where rnf = genericRnf
instance Binary RepoWebhookResponse

data PingEvent = PingEvent {
   pingEventZen    :: !Text
  ,pingEventHook   :: !RepoWebhook
  ,pingEventHookId :: !(Id RepoWebhook)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PingEvent where rnf = genericRnf
instance Binary PingEvent

data NewRepoWebhook = NewRepoWebhook {
  newRepoWebhookName   :: !Text
 ,newRepoWebhookConfig :: !(M.Map Text Text)
 ,newRepoWebhookEvents :: !(Maybe (Vector RepoWebhookEvent))
 ,newRepoWebhookActive :: !(Maybe Bool)
} deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData NewRepoWebhook where rnf = genericRnf
instance Binary NewRepoWebhook

data EditRepoWebhook = EditRepoWebhook {
  editRepoWebhookConfig       :: !(Maybe (M.Map Text Text))
 ,editRepoWebhookEvents       :: !(Maybe (Vector RepoWebhookEvent))
 ,editRepoWebhookAddEvents    :: !(Maybe (Vector RepoWebhookEvent))
 ,editRepoWebhookRemoveEvents :: !(Maybe (Vector RepoWebhookEvent))
 ,editRepoWebhookActive       :: !(Maybe Bool)
} deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData EditRepoWebhook where rnf = genericRnf
instance Binary EditRepoWebhook
