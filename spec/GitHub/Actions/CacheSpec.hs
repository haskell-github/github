{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.CacheSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson      (eitherDecodeStrict)
import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)
import qualified Data.Vector     as V
import           Test.Hspec      (Spec, describe, it, shouldBe)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding cache payloads" $ do
        it "decodes cache list payload" $ do
            V.length (GH.withTotalCountItems cacheList) `shouldBe` 1
        it "decodes cache usage for repo" $ do
            GH.repositoryCacheUsageFullName repoCacheUsage `shouldBe` "python/cpython"
            GH.repositoryCacheUsageActiveCachesSizeInBytes repoCacheUsage `shouldBe` 55000268087
            GH.repositoryCacheUsageActiveCachesCount repoCacheUsage `shouldBe` 171
        it "decodes cache usage for org" $ do
            GH.organizationCacheUsageTotalActiveCachesSizeInBytes orgCacheUsage `shouldBe` 26586
            GH.organizationCacheUsageTotalActiveCachesCount orgCacheUsage `shouldBe` 1

  where
    cacheList :: GH.WithTotalCount GH.Cache
    cacheList =
        fromRightS (eitherDecodeStrict cacheListPayload)

    repoCacheUsage :: GH.RepositoryCacheUsage
    repoCacheUsage =
        fromRightS (eitherDecodeStrict repoCacheUsagePayload)

    orgCacheUsage :: GH.OrganizationCacheUsage
    orgCacheUsage =
        fromRightS (eitherDecodeStrict orgCacheUsagePayload)

    cacheListPayload :: ByteString
    cacheListPayload = $(embedFile "fixtures/actions/cache-list.json")

    repoCacheUsagePayload :: ByteString
    repoCacheUsagePayload = $(embedFile "fixtures/actions/repo-cache-usage.json")

    orgCacheUsagePayload :: ByteString
    orgCacheUsagePayload = $(embedFile "fixtures/actions/org-cache-usage.json")
