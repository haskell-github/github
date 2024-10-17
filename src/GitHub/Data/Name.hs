module GitHub.Data.Name (
    Name(..),
    mkName,
    untagName,
    ) where

import Prelude ()
import GitHub.Internal.Prelude

import Data.Aeson.Types
       (FromJSONKey (..), ToJSONKey (..), fromJSONKeyCoerce, toJSONKeyText)

newtype Name entity = N Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

-- | Smart constructor for 'Name'
mkName :: proxy entity -> Text -> Name entity
mkName _ = N

untagName :: Name entity -> Text
untagName (N name) = name

instance Hashable (Name entity)
instance Binary (Name entity)

instance NFData (Name entity) where
    rnf (N s) = rnf s

instance FromJSON (Name entity) where
    parseJSON = fmap N . parseJSON

instance ToJSON (Name entity) where
    toJSON = toJSON . untagName

instance IsString (Name entity) where
    fromString = N . fromString

-- | @since 0.15.0.0
instance ToJSONKey (Name entity) where
    toJSONKey = toJSONKeyText untagName

-- | @since 0.15.0.0
instance FromJSONKey (Name entity) where
    fromJSONKey = fromJSONKeyCoerce
