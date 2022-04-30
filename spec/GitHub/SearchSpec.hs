{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.SearchSpec where

import Prelude ()
import Prelude.Compat

import Data.Aeson         (eitherDecodeStrict)
import Data.FileEmbed     (embedFile)
import Data.Proxy         (Proxy (..))
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe)

import qualified Data.Vector as V

import GitHub (github)
import GitHub.Data
       (Auth (..), Issue (..), IssueNumber (..), IssueState (..),
       SimpleUser (..), User, mkId)
import GitHub.Endpoints.Search (SearchResult' (..), SearchResult, searchIssuesR, searchUsersR)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (Auth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (OAuth $ fromString token)

spec :: Spec
spec = do
  describe "searchIssues" $ do
    it "decodes issue search response JSON" $ do
      let searchIssuesResult = fromRightS $ eitherDecodeStrict $(embedFile "fixtures/issue-search.json") :: SearchResult Issue
      searchResultTotalCount searchIssuesResult `shouldBe` 2

      let issues = searchResultResults searchIssuesResult
      V.length issues `shouldBe` 2

      let issue1 = issues V.! 0
      issueId issue1 `shouldBe` mkId (Proxy :: Proxy Issue) 123898390
      issueNumber issue1 `shouldBe` IssueNumber 130
      issueTitle issue1 `shouldBe` "Make test runner more robust"
      issueState issue1 `shouldBe` StateClosed

      let issue2 = issues V.! 1
      issueId issue2 `shouldBe` mkId (Proxy :: Proxy Issue) 119694665
      issueNumber issue2 `shouldBe` IssueNumber 127
      issueTitle issue2 `shouldBe` "Decouple request creation from execution"
      issueState issue2 `shouldBe` StateOpen

    it "performs an issue search via the API" $ withAuth $ \auth -> do
      let query = "Decouple in:title repo:haskell-github/github created:<=2015-12-01"
      issues <- fmap (searchResultResults . fromRightS) <$> github auth $ searchIssuesR query 5
      length issues `shouldBe` 1
      issueId (V.head issues) `shouldBe` mkId (Proxy :: Proxy Issue) 119694665

  describe "searchUsers" $
    it "performs a user search via the API" $ withAuth $ \auth -> do
      let query = "oleg.grenrus@iki.fi created:<2020-01-01"
      users <- fmap (searchResultResults . fromRightS) <$> github auth $ searchUsersR query 5
      length users `shouldBe` 1
      simpleUserId (V.head users) `shouldBe` mkId (Proxy :: Proxy User) 51087
