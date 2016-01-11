{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Id (
    Id(..),
    mkId,
    untagId,
    ) where

import Control.DeepSeq   (NFData (..))
import Data.Aeson.Compat (FromJSON (..), ToJSON (..))
import Data.Data         (Data, Typeable)
import Data.Hashable     (Hashable)
import GHC.Generics      (Generic)

-- | Numeric identifier.
newtype Id entity = Id Int
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

-- | Smart constructor for 'Id'.
mkId :: proxy entity -> Int -> Id entity
mkId _ = Id

untagId :: Id entity -> Int
untagId (Id name) = name

instance Hashable (Id entity)

instance NFData (Id entity) where
    rnf (Id s) = rnf s

instance FromJSON (Id entity) where
    parseJSON = fmap Id . parseJSON

instance ToJSON (Id entity) where
    toJSON = toJSON . untagId
