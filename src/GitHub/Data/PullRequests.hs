{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.PullRequests (
    SimplePullRequest(..),
    PullRequest(..),
    EditPullRequest(..),
    CreatePullRequest(..),
    PullRequestLinks(..),
    PullRequestCommit(..),
    PullRequestEvent(..),
    PullRequestEventType(..),
    PullRequestReference(..),
    PullRequestState(..),
    PullRequestSort(..),
    PullRequestSortDirection(..),
    -- * Pull Request listing options
    PullRequestOptions,
    defaultPullRequestOptions,
    pullRequestOptionsToQueryString,
    setPullRequestOptionsState,
    setPullRequestOptionsStateAll,
    setPullRequestOptionsSort,
    setPullRequestOptionsDirection,
    setPullRequestOptionsHead,
    setPullRequestOptionsBase,
    ) where

import Prelude        ()
import Prelude.Compat

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), ToJSON (..), Value (..), object,
                                 withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types         (typeMismatch)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Maybe               (catMaybes)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

import qualified Data.Text.Encoding as TE

data SimplePullRequest = SimplePullRequest
    { simplePullRequestClosedAt  :: !(Maybe UTCTime)
    , simplePullRequestCreatedAt :: !UTCTime
    , simplePullRequestUser      :: !SimpleUser
    , simplePullRequestPatchUrl  :: !URL
    , simplePullRequestState     :: !PullRequestState
    , simplePullRequestNumber    :: !Int
    , simplePullRequestHtmlUrl   :: !URL
    , simplePullRequestUpdatedAt :: !UTCTime
    , simplePullRequestBody      :: !Text
    , simplePullRequestIssueUrl  :: !Text
    , simplePullRequestDiffUrl   :: !URL
    , simplePullRequestUrl       :: !URL
    , simplePullRequestLinks     :: !PullRequestLinks
    , simplePullRequestMergedAt  :: !(Maybe UTCTime)
    , simplePullRequestTitle     :: !Text
    , simplePullRequestId        :: !(Id PullRequest)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimplePullRequest where rnf = genericRnf
instance Binary SimplePullRequest

data PullRequest = PullRequest
    { pullRequestClosedAt       :: !(Maybe UTCTime)
    , pullRequestCreatedAt      :: !UTCTime
    , pullRequestUser           :: !SimpleUser
    , pullRequestPatchUrl       :: !URL
    , pullRequestState          :: !PullRequestState
    , pullRequestNumber         :: !Int
    , pullRequestHtmlUrl        :: !URL
    , pullRequestUpdatedAt      :: !UTCTime
    , pullRequestBody           :: !Text
    , pullRequestIssueUrl       :: !Text
    , pullRequestDiffUrl        :: !URL
    , pullRequestUrl            :: !URL
    , pullRequestLinks          :: !PullRequestLinks
    , pullRequestMergedAt       :: !(Maybe UTCTime)
    , pullRequestTitle          :: !Text
    , pullRequestId             :: !(Id PullRequest)
    , pullRequestMergedBy       :: !(Maybe SimpleUser)
    , pullRequestChangedFiles   :: !Int
    , pullRequestHead           :: !PullRequestCommit
    , pullRequestComments       :: !Count
    , pullRequestDeletions      :: !Count
    , pullRequestAdditions      :: !Count
    , pullRequestReviewComments :: !Count
    , pullRequestBase           :: !PullRequestCommit
    , pullRequestCommits        :: !Count
    , pullRequestMerged         :: !Bool
    , pullRequestMergeable      :: !(Maybe Bool)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequest where rnf = genericRnf
instance Binary PullRequest

data EditPullRequest = EditPullRequest
    { editPullRequestTitle :: !(Maybe Text)
    , editPullRequestBody  :: !(Maybe Text)
    , editPullRequestState :: !(Maybe PullRequestState)
    } deriving (Show, Generic)

instance NFData EditPullRequest where rnf = genericRnf
instance Binary EditPullRequest

data CreatePullRequest =
      CreatePullRequest
      { createPullRequestTitle :: !Text
      , createPullRequestBody  :: !Text
      , createPullRequestHead  :: !Text
      , createPullRequestBase  :: !Text
      }
    | CreatePullRequestIssue
      { createPullRequestIssueNum :: !Int
      , createPullRequestHead     :: !Text
      , createPullRequestBase     :: !Text
      }
    deriving (Show, Generic)

instance NFData CreatePullRequest where rnf = genericRnf
instance Binary CreatePullRequest

data PullRequestLinks = PullRequestLinks
    { pullRequestLinksReviewComments :: !URL
    , pullRequestLinksComments       :: !URL
    , pullRequestLinksHtml           :: !URL
    , pullRequestLinksSelf           :: !URL
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestLinks where rnf = genericRnf
instance Binary PullRequestLinks

data PullRequestCommit = PullRequestCommit
    { pullRequestCommitLabel :: !Text
    , pullRequestCommitRef   :: !Text
    , pullRequestCommitSha   :: !Text
    , pullRequestCommitUser  :: !SimpleUser
    , pullRequestCommitRepo  :: !Repo
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestCommit where rnf = genericRnf
instance Binary PullRequestCommit

data PullRequestEvent = PullRequestEvent
    { pullRequestEventAction      :: !PullRequestEventType
    , pullRequestEventNumber      :: !Int
    , pullRequestEventPullRequest :: !PullRequest
    , pullRequestRepository       :: !Repo
    , pullRequestSender           :: !SimpleUser
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEvent where rnf = genericRnf
instance Binary PullRequestEvent

data PullRequestEventType
    = PullRequestOpened
    | PullRequestClosed
    | PullRequestSynchronized
    | PullRequestReopened
    | PullRequestAssigned
    | PullRequestUnassigned
    | PullRequestLabeled
    | PullRequestUnlabeled
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestEventType where rnf = genericRnf
instance Binary PullRequestEventType

data PullRequestReference = PullRequestReference
    { pullRequestReferenceHtmlUrl  :: !(Maybe Text)
    , pullRequestReferencePatchUrl :: !(Maybe Text)
    , pullRequestReferenceDiffUrl  :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestReference where rnf = genericRnf
instance Binary PullRequestReference

data PullRequestState
    = PullRequestStateOpen
    | PullRequestStateClosed
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestState where rnf = genericRnf
instance Binary PullRequestState

data PullRequestSort
    = PullRequestSortCreated
    | PulLRequestSortUpdated
    | PullRequestSortPopularity
    | PullRequestSortLongRunning
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestSort where rnf = genericRnf
instance Binary PullRequestSort

data PullRequestSortDirection
    = PullRequestSortDesc
    | PullRequestSortAsc
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance NFData PullRequestSortDirection where rnf = genericRnf
instance Binary PullRequestSortDirection

-- | See <https://developer.github.com/v3/pulls/#parameters>.
data PullRequestOptions = PullRequestOptions
    { pullRequestOptionsState     :: !(Maybe PullRequestState)
    , pullRequestOptionsHead      :: !(Maybe Text)
    , pullRequestOptionsBase      :: !(Maybe Text)
    , pullRequestOptionsSort      :: !PullRequestSort
    , pullRequestOptionsDirection :: !PullRequestSortDirection
    }

defaultPullRequestOptions :: PullRequestOptions
defaultPullRequestOptions = PullRequestOptions
    (Just PullRequestStateOpen)
    Nothing
    Nothing
    PullRequestSortCreated
    PullRequestSortDesc

setPullRequestOptionsState :: PullRequestState -> PullRequestOptions -> PullRequestOptions
setPullRequestOptionsState x opts = opts
    { pullRequestOptionsState = Just x }

setPullRequestOptionsStateAll :: PullRequestOptions -> PullRequestOptions
setPullRequestOptionsStateAll opts = opts
    { pullRequestOptionsState = Nothing }

setPullRequestOptionsSort :: PullRequestSort -> PullRequestOptions -> PullRequestOptions
setPullRequestOptionsSort x opts = opts
    { pullRequestOptionsSort = x }

setPullRequestOptionsDirection :: PullRequestSortDirection -> PullRequestOptions -> PullRequestOptions
setPullRequestOptionsDirection x opts = opts
    { pullRequestOptionsDirection = x }

setPullRequestOptionsHead :: Text -> PullRequestOptions -> PullRequestOptions
setPullRequestOptionsHead x opts = opts
    { pullRequestOptionsHead = Just x }

setPullRequestOptionsBase :: Text -> PullRequestOptions -> PullRequestOptions
setPullRequestOptionsBase x opts = opts
    { pullRequestOptionsBase = Just x }

pullRequestOptionsToQueryString :: PullRequestOptions -> QueryString
pullRequestOptionsToQueryString (PullRequestOptions state head_ base sort dir) =
    [ mk "state"     state'
    , mk "sort"      sort'
    , mk "direction" direction'
    ] ++ catMaybes
    [ mk "head" <$> head'
    , mk "base" <$> base'
    ]
  where
    mk k v = (k, Just v)
    state' = case state of
        Nothing                     -> "all"
        Just PullRequestStateOpen   -> "open"
        Just PullRequestStateClosed -> "closed"
    sort' = case sort of
        PullRequestSortCreated     -> "created"
        PulLRequestSortUpdated     -> "updated"
        PullRequestSortPopularity  -> "popularity"
        PullRequestSortLongRunning -> "long-running"
    direction' = case dir of
       PullRequestSortDesc -> "desc"
       PullRequestSortAsc  -> "asc"
    head' = fmap TE.encodeUtf8 head_
    base' = fmap TE.encodeUtf8 base

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON SimplePullRequest where
  parseJSON = withObject "SimplePullRequest" $ \o ->
      SimplePullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .:? "body" .!= "" -- TODO: no body is treated as empty
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"

instance ToJSON PullRequestState where
    toJSON PullRequestStateOpen = String "open"
    toJSON PullRequestStateClosed = String "closed"

instance FromJSON PullRequestState where
    parseJSON (String "open")   = pure PullRequestStateOpen
    parseJSON (String "closed") = pure PullRequestStateClosed
    parseJSON v                 = typeMismatch "PulLRequestState" v

instance ToJSON EditPullRequest where
    toJSON (EditPullRequest t b s) =
        object $ filter notNull [ "title" .= t, "body" .= b, "state" .= s ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

instance ToJSON CreatePullRequest where
    toJSON (CreatePullRequest t b headPR basePR) =
        object [ "title" .= t, "body" .= b, "head" .= headPR, "base" .= basePR ]
    toJSON (CreatePullRequestIssue issueNum headPR basePR) =
        object [ "issue" .= issueNum, "head" .= headPR, "base" .= basePR]

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o ->
      PullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
        <*> o .:? "merged_by"
        <*> o .: "changed_files"
        <*> o .: "head"
        <*> o .: "comments"
        <*> o .: "deletions"
        <*> o .: "additions"
        <*> o .: "review_comments"
        <*> o .: "base"
        <*> o .: "commits"
        <*> o .: "merged"
        <*> o .:? "mergeable"

instance FromJSON PullRequestLinks where
  parseJSON = withObject "PullRequestLinks" $ \o -> PullRequestLinks
      <$> fmap getHref (o .: "review_comments")
      <*> fmap getHref (o .: "comments")
      <*> fmap getHref (o .: "html")
      <*> fmap getHref (o .: "self")

instance FromJSON PullRequestCommit where
    parseJSON = withObject "PullRequestCommit" $ \o -> PullRequestCommit
        <$> o .: "label"
        <*> o .: "ref"
        <*> o .: "sha"
        <*> o .: "user"
        <*> o .: "repo"

instance FromJSON PullRequestEvent where
    parseJSON = withObject "PullRequestEvent" $ \o -> PullRequestEvent
        <$> o .: "action"
        <*> o .: "number"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "sender"

instance FromJSON PullRequestEventType where
    parseJSON (String "opened") = pure PullRequestOpened
    parseJSON (String "closed") = pure PullRequestClosed
    parseJSON (String "synchronize") = pure PullRequestSynchronized
    parseJSON (String "reopened") = pure PullRequestReopened
    parseJSON (String "assigned") = pure PullRequestAssigned
    parseJSON (String "unassigned") = pure PullRequestUnassigned
    parseJSON (String "labeled") = pure PullRequestLabeled
    parseJSON (String "unlabeled") = pure PullRequestUnlabeled
    parseJSON v = typeMismatch "Could not build a PullRequestEventType" v

instance FromJSON PullRequestReference where
    parseJSON = withObject "PullRequestReference" $ \o -> PullRequestReference
        <$> o .:? "html_url"
        <*> o .:? "patch_url"
        <*> o .:? "diff_url"

-- Helpers

newtype Href a = Href { getHref :: a }

instance FromJSON a => FromJSON (Href a) where
    parseJSON = withObject "href object" $
        \obj -> Href <$> obj .: "href"
