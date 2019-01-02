{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module GitHub.IssuesSpec where

import qualified GitHub

import Prelude ()
import Prelude.Compat

import Data.Either.Compat (isRight)
import Data.Foldable      (for_)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, expectationFailure, it, pendingWith, shouldSatisfy)

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
    describe "issuesForRepoR" $ do
        it "works" $ withAuth $ \auth -> for_ repos $ \(owner, repo) -> do
            GitHub.executeRequest auth $
                GitHub.issuesForRepoR owner repo mempty GitHub.FetchAll
            >>= \case
                Left err ->
                    expectationFailure $ show err
                Right issues ->
                    for_ issues $ \issue -> do
                        comments <- GitHub.executeRequest auth $
                            GitHub.commentsR owner repo (GitHub.issueNumber issue) 1
                        comments `shouldSatisfy` isRight

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      , ("haskell", "cabal")
      ]
