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
import System.Environment   (lookupEnv)
import Data.FileEmbed       (embedFile)
import Test.Hspec           (Spec, describe, it, pendingWith, shouldSatisfy, shouldBe)
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
        it "can parse PR json" $ do
            let pr :: Either String PullRequestEvent =
                  eitherDecodeStrict $(embedFile "fixtures/pull-request.json")
            pullRequestEventAction (fromRightS pr) `shouldBe` PullRequestOpened

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      , ("haskell", "cabal")
      ]
    opts = GitHub.stateClosed
