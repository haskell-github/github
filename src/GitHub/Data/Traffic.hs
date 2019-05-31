{-# LANGUAGE KindSignatures #-}

-- | Data types used in the traffic API
module GitHub.Data.Traffic where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import GitHub (Name)

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

data Event =
      View
    | Clone
    deriving (Eq, Show)

data Count (e :: Event) (p :: Period) = Count
    { countTimestamp :: !UTCTime
    , count :: !Int
    , countUniques :: !Int
    }

data Views p = Views
    { viewsCount :: !Int
    , viewsUniques :: !Int
    , viewsPer :: Vector (Count 'View p)
    }

data Clones p = Clones
    { clonesCount :: !Int
    , clonesUniques :: !Int
    , clonesPer :: Vector (Count 'Clone p)
    }
