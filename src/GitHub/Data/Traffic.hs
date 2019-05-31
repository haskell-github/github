{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Data types used in the traffic API
module GitHub.Data.Traffic where

import Data.Text   (Text)
import Data.Time   (UTCTime)
import Data.Vector (Vector)

import GitHub.Data.Name        (Name)
import GitHub.Internal.Prelude
import Prelude ()

data Referrer = Referrer
    { referrer        :: !(Name Referrer)
    , referrerCount   :: !Int
    , referrerUniques :: !Int
    }
    deriving (Eq, Show, Generic)

instance FromJSON Referrer where
  parseJSON = withObject "Referrer" $ \o ->
    Referrer
    <$> o .: "referrer"
    <*> o .: "count"
    <*> o .: "uniques"

data PopularPath = PopularPath
    { popularPath :: !Text
    , popularPathTitle :: !Text
    , popularPathCount :: !Int
    , popularPathUniques :: !Int
    }
    deriving (Eq, Show)

instance FromJSON PopularPath where
  parseJSON = withObject "Path" $ \o ->
    PopularPath
    <$> o .: "path"
    <*> o .: "title"
    <*> o .: "count"
    <*> o .: "uniques"

data Period' =
  Day'
  | Week'
  deriving (Eq, Show)

data Period p where
    Day :: Period 'Day'
    Week :: Period 'Week'

deriving instance Eq (Period p)
deriving instance Show (Period p)

prettyPeriod :: IsString a => Period p -> a
prettyPeriod = \case
  Day -> "day"
  Week -> "week"

data TrafficEvent =
      View
    | Clone
    deriving (Eq, Show)

data TrafficCount (e :: TrafficEvent) (p :: Period') = TrafficCount
    { trafficCountTimestamp :: !UTCTime
    , trafficCount :: !Int
    , trafficCountUniques :: !Int
    }
    deriving (Eq, Show)

instance FromJSON (TrafficCount e p) where
  parseJSON = withObject "TrafficCount" $ \o ->
    TrafficCount
    <$> o .: "timestamp"
    <*> o .: "count"
    <*> o .: "uniques"

data Views p = Views
    { viewsCount :: !Int
    , viewsUniques :: !Int
    , views :: !(Vector (TrafficCount 'View p))
    }
    deriving (Eq, Show)

instance FromJSON (Views p) where
  parseJSON = withObject "Views" $ \o ->
    Views
    <$> o .: "count"
    <*> o .: "uniques"
    <*> o .: "views"

data Clones p = Clones
    { clonesCount :: !Int
    , clonesUniques :: !Int
    , clones :: !(Vector (TrafficCount 'Clone p))
    }
    deriving (Eq, Show)

instance FromJSON (Clones p) where
  parseJSON = withObject "Clones" $ \o ->
    Clones
    <$> o .: "count"
    <*> o .: "uniques"
    <*> o .: "clones"
