{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

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
    parseJSON = withObject "Referrer" $ \o -> Referrer
        <$> o .: "referrer"
        <*> o .: "count"
        <*> o .: "uniques"

instance ToJSON Referrer where
    toJSON (Referrer r c u) = object
        [ "referrer" .= r
        , "count" .= c
        , "uniques" .= u
        ]

data PopularPath = PopularPath
    { popularPath        :: !Text
    , popularPathTitle   :: !Text
    , popularPathCount   :: !Int
    , popularPathUniques :: !Int
    }
    deriving (Eq, Show)

instance FromJSON PopularPath where
    parseJSON = withObject "Path" $ \o -> PopularPath
        <$> o .: "path"
        <*> o .: "title"
        <*> o .: "count"
        <*> o .: "uniques"

instance ToJSON PopularPath where
    toJSON (PopularPath p t c u) = object
        [ "path" .= p
        , "title" .= t
        , "count" .= c
        , "uniques" .= u
        ]

data Period =
    Day
    | Week
    deriving (Eq, Show)

data TrafficEvent
    = View
    | Clone
    deriving (Eq, Show)

data TrafficCount (e :: TrafficEvent) = TrafficCount
    { trafficCountTimestamp :: !UTCTime
    , trafficCount          :: !Int
    , trafficCountUniques   :: !Int
    }
    deriving (Eq, Show)

instance FromJSON (TrafficCount e) where
    parseJSON = withObject "TrafficCount" $ \o -> TrafficCount
        <$> o .: "timestamp"
        <*> o .: "count"
        <*> o .: "uniques"

instance ToJSON (TrafficCount e) where
    toJSON (TrafficCount t c u) = object
        [ "timestamp" .= t
        , "count" .= c
        , "uniques" .= u
        ]

data Views = Views
    { viewsCount   :: !Int
    , viewsUniques :: !Int
    , views        :: !(Vector (TrafficCount 'View))
    }
    deriving (Eq, Show)

instance FromJSON Views where
    parseJSON = withObject "Views" $ \o -> Views
        <$> o .: "count"
        <*> o .: "uniques"
        <*> o .: "views"

instance ToJSON Views where
    toJSON (Views c u v) = object
        [ "count" .= c
        , "uniques" .= u
        , "views" .= v
        ]

data Clones = Clones
    { clonesCount   :: !Int
    , clonesUniques :: !Int
    , clones        :: !(Vector (TrafficCount 'Clone))
    }
    deriving (Eq, Show)

instance FromJSON Clones where
    parseJSON = withObject "Clones" $ \o -> Clones
        <$> o .: "count"
        <*> o .: "uniques"
        <*> o .: "clones"

instance ToJSON Clones where
    toJSON (Clones c u cs) = object
        [ "count" .= c
        , "uniques" .= u
        , "clones" .= cs
        ]
