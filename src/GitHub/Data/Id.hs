-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Id (
    Id(..),
    mkId,
    untagId,
    ) where

import GitHub.Internal.Prelude
import Prelude ()

-- | Numeric identifier.
newtype Id entity = Id Int
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

-- | Smart constructor for 'Id'.
mkId :: proxy entity -> Int -> Id entity
mkId _ = Id

untagId :: Id entity -> Int
untagId (Id name) = name

instance Hashable (Id entity)
instance Binary (Id entity)

instance NFData (Id entity) where
    rnf (Id s) = rnf s

instance FromJSON (Id entity) where
    parseJSON = fmap Id . parseJSON

instance ToJSON (Id entity) where
    toJSON = toJSON . untagId
