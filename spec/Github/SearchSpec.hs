{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Github.SearchSpec where

import Control.Applicative ((<$>))
import Data.Aeson.Compat   (eitherDecodeStrict)
import Data.FileEmbed      (embedFile)
import Test.Hspec          (Spec, describe, it, shouldBe)

import Github.Data.Issues      (Issue(..), SearchIssuesResult(..))
import Github.Search           (searchIssues)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
  describe "searchIssues" $ do
    it "decodes issue search response JSON" $ do
      let searchIssuesResult = fromRightS $ eitherDecodeStrict $(embedFile "fixtures/issueSearch.json") :: SearchIssuesResult
      searchIssuesTotalCount searchIssuesResult `shouldBe` 2

      let issues = searchIssuesIssues searchIssuesResult
      length issues `shouldBe` 2

      let issue1 = head issues
      issueId issue1 `shouldBe` 123898390
      issueNumber issue1 `shouldBe` 130
      issueTitle issue1 `shouldBe` "Make test runner more robust"
      issueState issue1 `shouldBe` "closed"

      let issue2 = issues !! 1
      issueId issue2 `shouldBe` 119694665
      issueNumber issue2 `shouldBe` 127
      issueTitle issue2 `shouldBe` "Decouple request creation from execution"
      issueState issue2 `shouldBe` "open"

    it "performs an issue search via the API" $ do
      let query = "q=Decouple in:title repo:phadej/github created:<=2015-12-01"
      issues <- searchIssuesIssues . fromRightS <$> searchIssues query
      length issues `shouldBe` 1
      issueId (head issues) `shouldBe` 119694665
