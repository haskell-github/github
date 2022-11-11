-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}
module GitHub.Data.Actions.Secrets (
    Secret(..),
    PublicKey(..),
    SetSecret(..),
    SelectedRepo(..),
    SetSelectedRepositories(..),
    RepoSecret(..),
    Environment(..),
    ) where

import GitHub.Data.Id          (Id)
import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))
import GitHub.Data.Name (Name)
import GitHub.Data.Repos (Repo)
import Data.Maybe (maybeToList)


-------------------------------------------------------------------------------
-- Secret
-------------------------------------------------------------------------------

data Secret = Secret
    { secretNmae :: !(Name Secret)
    , secretCreatedAt :: !UTCTime
    , secretUpdatedAt :: !UTCTime
    , secretVisibility :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data PublicKey = PublicKey
    { publicKeyId :: !(Id PublicKey)
    , publicKeyKey :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data SetSecret = SetSecret 
    { setSecretPublicKeyId :: !(Id PublicKey)
    , setSecretEncryptedValue :: !Text
    , setSecretVisibility :: !Text
    , setSecretSelectedRepositoryIds :: !(Maybe [Id Repo])
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
    { repoSecretNmae :: !(Name Secret)
    , repoSecretCreatedAt :: !UTCTime
    , repoSecretUpdatedAt :: !UTCTime
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- TODO move somewhere else?
data Environment = Environment
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- -------------------------------------------------------------------------------
-- -- JSON instances
-- -------------------------------------------------------------------------------

instance FromJSON Secret where
    parseJSON = withObject "Secret" $ \o -> Secret
        <$> o .: "name"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "visibility"

instance FromJSON (WithTotalCount Secret) where
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

instance FromJSON (WithTotalCount SelectedRepo) where
    parseJSON = withObject "SelectedRepoList" $ \o -> WithTotalCount
        <$> o .: "repositories"
        <*> o .: "total_count"

instance FromJSON RepoSecret where
    parseJSON = withObject "RepoSecret" $ \o -> RepoSecret
        <$> o .: "name"
        <*> o .: "created_at"
        <*> o .: "updated_at"