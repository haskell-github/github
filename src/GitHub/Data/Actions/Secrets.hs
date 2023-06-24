{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Data.Actions.Secrets (
    OrganizationSecret(..),
    PublicKey(..),
    SetSecret(..),
    SetRepoSecret(..),
    SelectedRepo(..),
    SetSelectedRepositories(..),
    RepoSecret(..),
    Environment(..),
    ) where

import GitHub.Data.Id          (Id)
import GitHub.Internal.Prelude
import Prelude ()

import Data.Maybe                 (maybeToList)
import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Data.Name           (Name)
import GitHub.Data.Repos          (Repo)

-------------------------------------------------------------------------------
-- Secret
-------------------------------------------------------------------------------

data OrganizationSecret = OrganizationSecret
    { organizationSecretName :: !(Name OrganizationSecret)
    , organizationSecretCreatedAt :: !UTCTime
    , organizationSecretUpdatedAt :: !UTCTime
    , organizationSecretVisibility :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data PublicKey = PublicKey
    { publicKeyId :: !Text
    , publicKeyKey :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data SetSecret = SetSecret
    { setSecretPublicKeyId :: !Text
    , setSecretEncryptedValue :: !Text
    , setSecretVisibility :: !Text
    , setSecretSelectedRepositoryIds :: !(Maybe [Id Repo])
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data SetRepoSecret = SetRepoSecret
    { setRepoSecretPublicKeyId :: !Text
    , setRepoSecretEncryptedValue :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data SelectedRepo = SelectedRepo
    { selectedRepoRepoId :: !(Id Repo)
    , selectedRepoRepoName :: !(Name Repo)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data SetSelectedRepositories = SetSelectedRepositories
    { setSelectedRepositoriesRepositoryIds :: ![Id Repo]
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data RepoSecret = RepoSecret
    { repoSecretName :: !(Name RepoSecret)
    , repoSecretCreatedAt :: !UTCTime
    , repoSecretUpdatedAt :: !UTCTime
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- TODO move somewhere else?
data Environment = Environment
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON OrganizationSecret where
    parseJSON = withObject "Secret" $ \o -> OrganizationSecret
        <$> o .: "name"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "visibility"

instance FromJSON (WithTotalCount OrganizationSecret) where
    parseJSON = withObject "SecretList" $ \o -> WithTotalCount
        <$> o .: "secrets"
        <*> o .: "total_count"

instance FromJSON PublicKey where
    parseJSON = withObject "PublicKey" $ \o -> PublicKey
        <$> o .: "key_id"
        <*> o .: "key"

instance FromJSON SelectedRepo where
    parseJSON = withObject "SelectedRepo" $ \o -> SelectedRepo
        <$> o .: "id"
        <*> o .: "name"

instance ToJSON SetSelectedRepositories where
    toJSON SetSelectedRepositories{..} =
        object
            [ "selected_repository_ids" .=  setSelectedRepositoriesRepositoryIds
            ]

instance ToJSON SetSecret where
    toJSON SetSecret{..} =
        object $
            [ "encrypted_value" .= setSecretEncryptedValue
            ,  "key_id" .= setSecretPublicKeyId
            ,  "visibility" .= setSecretVisibility
            ] <> maybeToList (fmap ("selected_repository_ids" .=) setSecretSelectedRepositoryIds)

instance ToJSON SetRepoSecret where
    toJSON SetRepoSecret{..} =
        object
            [ "encrypted_value" .= setRepoSecretEncryptedValue
            ,  "key_id" .= setRepoSecretPublicKeyId
            ]

instance FromJSON (WithTotalCount SelectedRepo) where
    parseJSON = withObject "SelectedRepoList" $ \o -> WithTotalCount
        <$> o .: "repositories"
        <*> o .: "total_count"

instance FromJSON RepoSecret where
    parseJSON = withObject "RepoSecret" $ \o -> RepoSecret
        <$> o .: "name"
        <*> o .: "created_at"
        <*> o .: "updated_at"

instance FromJSON (WithTotalCount RepoSecret) where
    parseJSON = withObject "RepoSecretList" $ \o -> WithTotalCount
        <$> o .: "secrets"
        <*> o .: "total_count"
