{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Github.SearchSpec where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat (eitherDecodeStrict)
import Data.FileEmbed    (embedFile)
import Test.Hspec        (Spec, describe, it, shouldBe)

import qualified Data.Vector as V

import Github.Data.Id     (Id (..))
import Github.Data.Issues (Issue (..))
import Github.Search      (SearchIssuesResult (..), searchIssues)

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
      V.length issues `shouldBe` 2

      let issue1 = issues V.! 0
      issueId issue1 `shouldBe` Id 123898390
      issueNumber issue1 `shouldBe` 130
      issueTitle issue1 `shouldBe` "Make test runner more robust"
      issueState issue1 `shouldBe` "closed"

      let issue2 = issues V.! 1
      issueId issue2 `shouldBe` Id 119694665
      issueNumber issue2 `shouldBe` 127
      issueTitle issue2 `shouldBe` "Decouple request creation from execution"
      issueState issue2 `shouldBe` "open"

    it "performs an issue search via the API" $ do
      let query = "Decouple in:title repo:phadej/github created:<=2015-12-01"
      issues <- searchIssuesIssues . fromRightS <$> searchIssues query
      length issues `shouldBe` 1
      issueId (V.head issues) `shouldBe` Id 119694665
