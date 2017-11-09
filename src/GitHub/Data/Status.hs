{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module GitHub.Data.Status where

import GitHub.Data.Definitions
import GitHub.Data.URL
import GitHub.Internal.Prelude
import Prelude ()

data StatusState = Error | Failure | Pending | Success
  deriving (Enum, Eq, Ord, Read, Show, Bounded)

data NewStatus = NewStatus
    { newStatusState :: StatusState
    , newStatusTargetUrl :: Maybe URL
    , newStatusDescription :: Maybe Text
    , newStatusContext :: Text
    }
  deriving (Eq, Show)

data Status = Status
  { statusCreatedAt :: UTCTime
  , statusUpdatedAt :: UTCTime
  , statusState :: StatusState
  , statusTargetUrl :: Maybe URL
  , statusDescription :: Maybe Text
  , statusId :: Int
  , statusUrl :: URL
  , statusContext :: Text
  , statusCreator :: SimpleUser
  }
  deriving (Eq, Show)

-- JSON Instances

instance ToJSON StatusState where
  toJSON Error = "error"
  toJSON Failure = "failure"
  toJSON Pending = "pending"
  toJSON Success = "success"

instance FromJSON StatusState where
  parseJSON = withText "StatusState" $ \t -> 
    case t of
      "error" -> return Error
      "failure" -> return Failure
      "pending" -> return Pending
      "success" -> return Success
      _ -> fail $ "Invalid StatusState: " ++ unpack t

instance ToJSON NewStatus where
  toJSON ns = object [ "state" .= newStatusState ns
                     , "target_url" .= newStatusTargetUrl ns
                     , "description" .= newStatusDescription ns
                     , "context" .= newStatusContext ns
                     ]

instance FromJSON Status where
  parseJSON = withObject "Status" $ \o ->
    Status <$> o .: "created_at"
           <*> o .: "updated_at"
           <*> o .: "state"
           <*> o .: "target_url"
           <*> o .: "description"
           <*> o .: "id"
           <*> o .: "url"
           <*> o .: "context"
           <*> o .: "creator"
