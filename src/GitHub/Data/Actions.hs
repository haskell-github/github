-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module GitHub.Data.Actions (
    Artifact(..),
    ArtifactList(..),
    Workflow,
    PaginatedWithTotalCount(..),
    WithTotalCount(..),
    WorkflowRun,
    ) where

import GHC.TypeLits
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import           Data.Data (Proxy (..))
import qualified Data.Text as T

data Artifact = Artifact
    { artifactArchiveDownloadUrl :: !URL
    , artifactCreatedAt :: !UTCTime
    , artifactExpired :: !Bool
    , artifactExpiresAt :: !UTCTime
    , artifactId :: !(Id Artifact)
    , artifactName :: !Text
    , artifactNodeId :: !Text
    , artifactSizeInBytes :: !Int
    , artifactUpdatedAt :: !UTCTime
    , artifactUrl :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data ArtifactList = ArtifactList
    { artifactListArtifacts :: !(Vector Artifact)
    , artifactListTotalCount :: !Int
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

data PaginatedWithTotalCount a (tag :: Symbol) = PaginatedWithTotalCount
    { paginatedWithTotalCountItems :: !(Vector a)
    , paginatedWithTotalCountTotalCount :: !Int
    }

data WithTotalCount a = WithTotalCount
    { withTotalCountItems :: !(Vector a)
    , withTotalCountTotalCount :: !Int
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance Semigroup (WithTotalCount a) where
    (WithTotalCount items1 count1) <> (WithTotalCount items2 _) =
        WithTotalCount (items1 <> items2) count1

instance Foldable WithTotalCount where
    foldMap f (WithTotalCount items _) = foldMap f items

data Workflow
data WorkflowRun

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON Artifact where
    parseJSON = withObject "Artifact" $ \o -> Artifact
        <$> o .: "archive_download_url"
        <*> o .: "created_at"
        <*> o .: "expired"
        <*> o .: "expires_at"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "node_id"
        <*> o .: "size_in_bytes"
        <*> o .: "updated_at"
        <*> o .: "url"

instance FromJSON ArtifactList where
    parseJSON = withObject "ArtifactList" $ \o -> ArtifactList
        <$> o .: "artifacts"
        <*> o .: "total_count"

instance (FromJSON a, KnownSymbol l) => FromJSON (PaginatedWithTotalCount a l) where
    parseJSON = withObject "PaginatedWithTotalCount" $ \o -> PaginatedWithTotalCount
        <$> o .: T.pack (symbolVal (Proxy :: Proxy l))
        <*> o .: "total_count"

instance FromJSON (WithTotalCount Artifact) where
    parseJSON = withObject "ArtifactList" $ \o -> WithTotalCount
        <$> o .: "artifacts"
        <*> o .: "total_count"
