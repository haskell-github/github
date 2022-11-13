-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module GitHub.Data.Actions.Cache (
    Cache(..),
    RepositoryCacheUsage(..),
    OrganizationCacheUsage(..)
    ) where

import GitHub.Data.Id          (Id)
import GitHub.Internal.Prelude
import Prelude ()

import GitHub.Data.Actions.Common (WithTotalCount (WithTotalCount))


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

data RepositoryCacheUsage = RepositoryCacheUsage
              { repositoryCacheUsageFullName :: !Text
              , repositoryCacheUsageActiveCachesSizeInBytes :: !Int
              , repositoryCacheUsageActiveCachesCount :: !Int
              }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data OrganizationCacheUsage = OrganizationCacheUsage
              { organizationCacheUsageTotalActiveCachesSizeInBytes :: !Int
              , organizationCacheUsageTotalActiveCachesCount :: !Int
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
    parseJSON = withObject "RepositoryCacheUsage" $ \o -> RepositoryCacheUsage
        <$> o .: "full_name"
        <*> o .: "active_caches_size_in_bytes"
        <*> o .: "active_caches_count"

instance FromJSON (WithTotalCount RepositoryCacheUsage) where
    parseJSON = withObject "CacheUsageList" $ \o -> WithTotalCount
        <$> o .: "repository_cache_usages"
        <*> o .: "total_count"
