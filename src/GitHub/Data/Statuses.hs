{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module GitHub.Data.Statuses where

import GitHub.Data.Definitions
import GitHub.Data.Name        (Name)
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Data.GitData (Commit)
import GitHub.Data.Repos   (RepoRef)


data StatusState
    = StatusPending
    | StatusSuccess
    | StatusError
    | StatusFailure
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData StatusState where rnf = genericRnf
instance Binary StatusState

instance FromJSON StatusState where
  parseJSON (String "pending") = pure StatusPending
  parseJSON (String "success") = pure StatusSuccess
  parseJSON (String "error")   = pure StatusError
  parseJSON (String "failure") = pure StatusFailure
  parseJSON _ = fail "Could not build a StatusState"

instance ToJSON StatusState where
  toJSON StatusPending = String "pending"
  toJSON StatusSuccess = String "success"
  toJSON StatusError   = String "error"
  toJSON StatusFailure = String "failure"


data Status = Status
    { statusCreatedAt   :: !UTCTime
    , statusUpdatedAt   :: !UTCTime
    , statusState       :: !StatusState
    , statusTargetUrl   :: !(Maybe URL)
    , statusDescription :: !(Maybe Text)
    , statusId          :: !(Id Status)
    , statusUrl         :: !URL
    , statusContext     :: !(Maybe Text)
    , statusCreator     :: !(Maybe SimpleUser)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON Status where
  parseJSON = withObject "Status" $ \o -> Status
      <$> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "state"
      <*> o .:? "target_url"
      <*> o .:? "description"
      <*> o .: "id"
      <*> o .: "url"
      <*> o .:? "context"
      <*> o .:? "creator"


data NewStatus = NewStatus
    { newStatusState       :: !StatusState
    , newStatusTargetUrl   :: !(Maybe URL)
    , newStatusDescription :: !(Maybe Text)
    , newStatusContext     :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewStatus where rnf = genericRnf
instance Binary NewStatus

instance ToJSON NewStatus where
    toJSON (NewStatus s t d c) = object $ filter notNull $
        [ "state"       .= s
        , "target_url"  .= t
        , "description" .= d
        , "context"     .= c
        ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True


data CombinedStatus = CombinedStatus
    { combinedStatusState      :: !StatusState
    , combinedStatusSha        :: !(Name Commit)
    , combinedStatusTotalCount :: !Int
    , combinedStatusStatuses   :: !(Vector Status)
    , combinedStatusRepository :: !RepoRef
    , combinedStatusCommitUrl  :: !URL
    , combinedStatusUrl        :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON CombinedStatus where
    parseJSON = withObject "CombinedStatus" $ \o -> CombinedStatus
        <$> o .: "state"
        <*> o .: "sha"
        <*> o .: "total_count"
        <*> o .: "statuses"
        <*> o .: "repository"
        <*> o .: "commit_url"
        <*> o .: "url"
