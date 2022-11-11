{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.SecretsSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.Either.Compat (isRight)
import           Data.FileEmbed     (embedFile)
import           Data.Foldable      (for_)
import           Data.String        (fromString)
import qualified Data.Vector        as V
import           System.Environment (lookupEnv)
import           Test.Hspec
                 (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding secrets payloads" $ do
        it "decodes selected repo list payload" $ do
            V.length (GH.withTotalCountItems repoList) `shouldBe` 1
        -- it "decodes cache usage for repo" $ do
        --     GH.repositoryCacheUsageFullName repoCacheUsage `shouldBe` "python/cpython"
        --     GH.repositoryCacheUsageActiveCachesSizeInBytes repoCacheUsage `shouldBe` 55000268087
        --     GH.repositoryCacheUsageActiveCachesCount repoCacheUsage `shouldBe` 171
        -- it "decodes cache usage for org" $ do
        --     GH.organizationCacheUsageTotalActiveCachesSizeInBytes orgCacheUsage `shouldBe` 26586
        --     GH.organizationCacheUsageTotalActiveCachesCount orgCacheUsage `shouldBe` 1

  where
    repoList :: GH.WithTotalCount GH.SelectedRepo
    repoList =
        fromRightS (eitherDecodeStrict repoListPayload)

    -- repoCacheUsage :: GH.RepositoryCacheUsage
    -- repoCacheUsage =
    --     fromRightS (eitherDecodeStrict repoCacheUsagePayload)

    -- orgCacheUsage :: GH.OrganizationCacheUsage
    -- orgCacheUsage =
    --     fromRightS (eitherDecodeStrict orgCacheUsagePayload)

    repoListPayload :: ByteString
    repoListPayload = $(embedFile "fixtures/actions/selected-repositories-for-secret.json")

    -- repoCacheUsagePayload :: ByteString
    -- repoCacheUsagePayload = $(embedFile "fixtures/actions/repo-cache-usage.json")

    -- orgCacheUsagePayload :: ByteString
    -- orgCacheUsagePayload = $(embedFile "fixtures/actions/org-cache-usage.json")