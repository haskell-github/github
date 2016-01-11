{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Name (
    Name(..),
    mkName,
    untagName,
    ) where

import Control.DeepSeq   (NFData (..))
import Data.Aeson.Compat (FromJSON (..), ToJSON (..))
import Data.Data         (Data, Typeable)
import Data.Hashable     (Hashable)
import Data.String       (IsString (..))
import Data.Text         (Text)
import GHC.Generics      (Generic)

newtype Name entity = N Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

-- | Smart constructor for 'Name'
mkName :: proxy entity -> Text -> Name entity
mkName _ = N

untagName :: Name entity -> Text
untagName (N name) = name

instance Hashable (Name entity)

instance NFData (Name entity) where
    rnf (N s) = rnf s

instance FromJSON (Name entity) where
    parseJSON = fmap N . parseJSON

instance ToJSON (Name entity) where
    toJSON = toJSON . untagName

instance IsString (Name entity) where
    fromString = N . fromString
