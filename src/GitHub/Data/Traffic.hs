{-# LANGUAGE KindSignatures #-}

-- | Data types used in the traffic API
module GitHub.Data.Traffic where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import GitHub.Data.Name (Name)

data Referrer = Referrer
    { referrer        :: !(Name Referrer)
    , referrerCount   :: !Int
    , referrerUniques :: !Int
    }
    deriving (Eq, Show)

data Path = Path
    { path :: !Text
    , pathTitle :: !Text
    , pathCount :: !Int
    , pathUniques :: !Int
    }
    deriving (Eq, Show)

data Period =
      Day
    | Week
    deriving (Eq, Show)

data TrafficEvent =
      View
    | Clone
    deriving (Eq, Show)

data TrafficCount (e :: TrafficEvent) (p :: Period) = TrafficCount
    { trafficCountTimestamp :: !UTCTime
    , trafficCount :: !Int
    , trafficCountUniques :: !Int
    }

data Views p = Views
    { viewsCount :: !Int
    , viewsUniques :: !Int
    , viewsPer :: Vector (TrafficCount 'View p)
    }

data Clones p = Clones
    { clonesCount :: !Int
    , clonesUniques :: !Int
    , clonesPer :: Vector (TrafficCount 'Clone p)
    }
