-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
module GitHub.Data.DeployKeys where

import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data RepoDeployKey = RepoDeployKey
    { repoDeployKeyId        :: !(Id RepoDeployKey)
    , repoDeployKeyKey       :: !Text
    , repoDeployKeyUrl       :: !URL
    , repoDeployKeyTitle     :: !Text
    , repoDeployKeyVerified  :: !Bool
    , repoDeployKeyCreatedAt :: !UTCTime
    , repoDeployKeyReadOnly  :: !Bool
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON RepoDeployKey where
    parseJSON = withObject "RepoDeployKey" $ \o -> RepoDeployKey
        <$> o .: "id"
        <*> o .: "key"
        <*> o .: "url"
        <*> o .: "title"
        <*> o .: "verified"
        <*> o .: "created_at"
        <*> o .: "read_only"

data NewRepoDeployKey = NewRepoDeployKey
    { newRepoDeployKeyKey      :: !Text
    , newRepoDeployKeyTitle    :: !Text
    , newRepoDeployKeyReadOnly :: !Bool
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance ToJSON NewRepoDeployKey where
    toJSON (NewRepoDeployKey key title readOnly) = object
        [ "key" .= key
        , "title" .= title
        , "read_only" .= readOnly
        ]

instance FromJSON NewRepoDeployKey where
    parseJSON = withObject "RepoDeployKey" $ \o -> NewRepoDeployKey
        <$> o .: "key"
        <*> o .: "title"
        <*> o .: "read_only"
