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
    Workflow,
    PaginatedWithTotalCount(..),
    WithTotalCount(..),
    WorkflowRun,
    Cache(..),
    RepositoryCacheUsage(..),
    OrganizationCacheUsage(..),
    ) where

import GHC.TypeLits
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import           Data.Data (Proxy (..))
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

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


-------------------------------------------------------------------------------
-- Artifact
-------------------------------------------------------------------------------

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

instance (FromJSON a, KnownSymbol l) => FromJSON (PaginatedWithTotalCount a l) where
    parseJSON = withObject "PaginatedWithTotalCount" $ \o -> PaginatedWithTotalCount
        <$> o .: T.pack (symbolVal (Proxy :: Proxy l))
        <*> o .: "total_count"

instance FromJSON (WithTotalCount Artifact) where
    parseJSON = withObject "ArtifactList" $ \o -> WithTotalCount
        <$> o .: "artifacts"
        <*> o .: "total_count"


-------------------------------------------------------------------------------
-- Cache
-------------------------------------------------------------------------------

data Cache = Cache
    { cacheId :: !(Id Cache)
    , cacheRef :: !Text
    , cacheKey :: !Text
    , cacheVersion :: !Text
    , cacheLastAccessedAt :: !UTCTime
    , cacheCreatedAt :: !UTCTime
    , cacheSizeInBytes :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data RepositoryCacheUsage = CacheUsage
              { fullName :: !Text
              , activeCachesSizeInBytes :: !Int
              , activeCachesCount :: !Int
              }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data OrganizationCacheUsage = OrganizationCacheUsage
              { totalActiveCachesSizeInBytes :: !Int
              , totalActiveCachesCount :: !Int
              }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- -------------------------------------------------------------------------------
-- -- JSON instances
-- -------------------------------------------------------------------------------

instance FromJSON Cache where
    parseJSON = withObject "Cache" $ \o -> Cache
        <$> o .: "id"
        <*> o .: "ref"
        <*> o .: "key"
        <*> o .: "version"
        <*> o .: "last_accessed_at"
        <*> o .: "created_at"
        <*> o .: "size_in_bytes"

instance FromJSON (WithTotalCount Cache) where
    parseJSON = withObject "CacheList" $ \o -> WithTotalCount
        <$> o .: "actions_caches"
        <*> o .: "total_count"

instance FromJSON OrganizationCacheUsage where
    parseJSON = withObject "OrganizationCacheUsage" $ \o -> OrganizationCacheUsage
        <$> o .: "total_active_caches_size_in_bytes"
        <*> o .: "total_active_caches_count"

instance FromJSON RepositoryCacheUsage where
    parseJSON = withObject "CacheUsage" $ \o -> CacheUsage
        <$> o .: "full_name"
        <*> o .: "active_caches_size_in_bytes"
        <*> o .: "active_caches_count"

instance FromJSON (WithTotalCount RepositoryCacheUsage) where
    parseJSON = withObject "CacheUsageList" $ \o -> WithTotalCount
        <$> o .: "repository_cache_usages"
        <*> o .: "total_count"