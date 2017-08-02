{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.PullRequestsSpec where

import qualified GitHub

import Prelude ()
import Prelude.Compat

import Data.Aeson.Compat    (eitherDecodeStrict)
import Data.Either.Compat   (isRight)
import Data.Foldable        (for_)
import Data.String          (fromString)
import qualified Data.Vector as V
import System.Environment   (lookupEnv)
import Data.FileEmbed       (embedFile)
import Test.Hspec           (Spec, describe, it, pendingWith, shouldSatisfy, shouldBe)
import GitHub.Data.Definitions (simpleUserLogin)
import GitHub.Data.Options  (IssueState(..))
import GitHub.Data.PullRequests

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
    describe "PullRequest" $ do
        it "can parse PR json, from a PR to the github repo" $ do
            let pr = fromRightS $
                  eitherDecodeStrict $(embedFile "fixtures/pull-request-realworld.json")

            pullRequestState pr `shouldBe` StateOpen
            simpleUserLogin (pullRequestUser pr) `shouldBe` "adnelson"
            let [user] = V.toList (pullRequestAssignees pr)
            simpleUserLogin user `shouldBe` "phadej"
        it "can parse PR json, from the 'baxterthehacker' account" $ do
            let pr = fromRightS $
                  eitherDecodeStrict $(embedFile "fixtures/pull-request-baxterthehacker.json")
            pullRequestState pr `shouldBe` StateClosed
            simpleUserLogin (pullRequestUser pr) `shouldBe` "baxterthehacker"
    describe "PullRequestEvent" $ do
        it "can parse PR event json, example from github docs" $ do
            let pre = fromRightS $
                  eitherDecodeStrict $(embedFile "fixtures/pull-request-event-github-example.json")
            pullRequestEventAction pre `shouldBe` PullRequestOpened
            simpleUserLogin (pullRequestSender pre) `shouldBe` "baxterthehacker"
        it "can parse PR event json, example from a real-world github repo" $ do
            let pre = fromRightS $
                  eitherDecodeStrict $(embedFile "fixtures/pull-request-event-realworld.json")
            pullRequestEventAction pre `shouldBe` PullRequestOpened
            simpleUserLogin (pullRequestSender pre) `shouldBe` "adnelson"

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      , ("haskell", "cabal")
      ]
    opts = GitHub.stateClosed
