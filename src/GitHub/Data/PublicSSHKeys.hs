-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
module GitHub.Data.PublicSSHKeys where

import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data PublicSSHKeyBasic = PublicSSHKeyBasic
    { basicPublicSSHKeyId        :: !(Id PublicSSHKey)
    , basicPublicSSHKeyKey       :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON PublicSSHKeyBasic where
    parseJSON = withObject "PublicSSHKeyBasic" $ \o -> PublicSSHKeyBasic
        <$> o .: "id"
        <*> o .: "key"

data PublicSSHKey = PublicSSHKey
    { publicSSHKeyId        :: !(Id PublicSSHKey)
    , publicSSHKeyKey       :: !Text
    , publicSSHKeyUrl       :: !URL
    , publicSSHKeyTitle     :: !Text
    , publicSSHKeyVerified  :: !Bool
    , publicSSHKeyCreatedAt :: !(Maybe UTCTime)
    , publicSSHKeyReadOnly  :: !Bool
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON PublicSSHKey where
    parseJSON = withObject "PublicSSHKey" $ \o -> PublicSSHKey
        <$> o .: "id"
        <*> o .: "key"
        <*> o .: "url"
        <*> o .: "title"
        <*> o .: "verified"
        <*> o .:? "created_at"
        <*> o .: "read_only"

data NewPublicSSHKey = NewPublicSSHKey
    { newPublicSSHKeyKey      :: !Text
    , newPublicSSHKeyTitle    :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance ToJSON NewPublicSSHKey where
    toJSON (NewPublicSSHKey key title) = object
        [ "key" .= key
        , "title" .= title
        ]

instance FromJSON NewPublicSSHKey where
    parseJSON = withObject "PublicSSHKey" $ \o -> NewPublicSSHKey
        <$> o .: "key"
        <*> o .: "title"
