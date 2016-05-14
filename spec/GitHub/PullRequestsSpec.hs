{-# LANGUAGE OverloadedStrings #-}
module GitHub.PullRequestsSpec where

import qualified GitHub

import Data.Either.Compat (isRight)
import Data.Foldable      (for_)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

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
                GitHub.pullRequestsForR owner repo (Just "closed") Nothing
            cs `shouldSatisfy` isRight
  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      , ("haskell", "cabal")
      ]
