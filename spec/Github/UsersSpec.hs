{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Github.UsersSpec where

import Control.Applicative ((<$>))
import Data.Aeson.Compat   (eitherDecodeStrict)
import Data.Either.Compat  (isRight)
import Data.FileEmbed      (embedFile)
import System.Environment  (lookupEnv)
import Test.Hspec          (Spec, describe, it, pendingWith, shouldBe,
                            shouldSatisfy)

import Github.Auth             (GithubAuth (..))
import Github.Data.Definitions (DetailedOwner (..))
import Github.Data.Issues      (Issue(..), SearchIssuesResult(..))
import Github.Search           (searchIssues)
import Github.Users            (userInfoCurrent', userInfoFor')

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (GithubAuth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (GithubOAuth token)

spec :: Spec
spec = do
  describe "userInfoFor" $ do
    it "decodes user json" $ do
      let userInfo = eitherDecodeStrict $(embedFile "fixtures/user.json")
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "returns information about the user" $ withAuth $ \auth -> do
      userInfo <- userInfoFor' (Just auth) "mike-burns"
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

  describe "userInfoCurrent'" $ do
    it "returns information about the autenticated user" $ withAuth $ \auth -> do
      userInfo <- userInfoCurrent' (Just auth)
      userInfo `shouldSatisfy` isRight

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
