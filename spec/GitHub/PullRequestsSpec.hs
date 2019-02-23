{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.PullRequestsSpec where

import qualified GitHub

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.Either.Compat (isRight)
import           Data.FileEmbed     (embedFile)
import           Data.Foldable      (for_)
import           Data.String        (fromString)
import qualified Data.Vector        as V
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Environment (lookupEnv)
import           Test.Hspec
                 (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (GitHub.Auth -> IO ()) -> IO ()
withAuth action = do
    mtoken <- lookupEnv "GITHUB_TOKEN"
    case mtoken of
        Nothing    -> pendingWith "no GITHUB_TOKEN"
        Just token -> action (GitHub.OAuth $ fromString token)

spec :: Spec
spec = do
    describe "pullRequestsForR" $ do
        it "works" $ withAuth $ \auth -> for_ repos $ \(owner, repo) -> do
            cs <- GitHub.executeRequest auth $
                GitHub.pullRequestsForR owner repo opts GitHub.FetchAll
            cs `shouldSatisfy` isRight

    describe "pullRequestPatchR" $
        it "works" $ withAuth $ \auth -> do
            Right patch <- GitHub.executeRequest auth $
                GitHub.pullRequestPatchR "phadej" "github" (GitHub.IssueNumber 349)
            head (LBS8.lines patch) `shouldBe` "From c0e4ad33811be82e1f72ee76116345c681703103 Mon Sep 17 00:00:00 2001"

    describe "decoding pull request payloads" $ do
        it "decodes a pull request 'opened' payload" $ do
            V.length (GitHub.simplePullRequestRequestedReviewers simplePullRequestOpened)
                `shouldBe` 0

            V.length (GitHub.pullRequestRequestedReviewers pullRequestOpened)
                `shouldBe` 0

        it "decodes a pull request 'review_requested' payload" $ do
            V.length (GitHub.simplePullRequestRequestedReviewers simplePullRequestReviewRequested)
                `shouldBe` 1

            V.length (GitHub.pullRequestRequestedReviewers pullRequestReviewRequested)
                `shouldBe` 1

    describe "checking if a pull request is merged" $ do
        it "works" $ withAuth $ \auth -> do
            b <- GitHub.executeRequest auth $ GitHub.isPullRequestMergedR "phadej" "github" (GitHub.IssueNumber 14)
            b `shouldSatisfy` isRight
            fromRightS b `shouldBe` True

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      ]
    opts = GitHub.stateClosed

    simplePullRequestOpened :: GitHub.SimplePullRequest
    simplePullRequestOpened =
        fromRightS (eitherDecodeStrict prOpenedPayload)

    pullRequestOpened :: GitHub.PullRequest
    pullRequestOpened =
        fromRightS (eitherDecodeStrict prOpenedPayload)

    simplePullRequestReviewRequested :: GitHub.SimplePullRequest
    simplePullRequestReviewRequested =
        fromRightS (eitherDecodeStrict prReviewRequestedPayload)

    pullRequestReviewRequested :: GitHub.PullRequest
    pullRequestReviewRequested =
        fromRightS (eitherDecodeStrict prReviewRequestedPayload)

    prOpenedPayload :: ByteString
    prOpenedPayload = $(embedFile "fixtures/pull-request-opened.json")

    prReviewRequestedPayload :: ByteString
    prReviewRequestedPayload = $(embedFile "fixtures/pull-request-review-requested.json")
