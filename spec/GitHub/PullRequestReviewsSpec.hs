{-# LANGUAGE OverloadedStrings #-}
module GitHub.PullRequestReviewsSpec where

import qualified GitHub
import GitHub.Data.Id (Id(Id))

import Prelude ()
import Prelude.Compat

import Data.Either.Compat   (isRight)
import Data.Foldable        (for_)
import Data.String          (fromString)
import System.Environment   (lookupEnv)
import Test.Hspec           (Spec, describe, it, pendingWith, shouldSatisfy)

withAuth :: (GitHub.Auth -> IO ()) -> IO ()
withAuth action = do
    mtoken <- lookupEnv "GITHUB_TOKEN"
    case mtoken of
        Nothing    -> pendingWith "no GITHUB_TOKEN"
        Just token -> action (GitHub.OAuth $ fromString token)

spec :: Spec
spec = do
    describe "pullRequestReviewsR" $ do
        it "works" $ withAuth $ \auth -> for_ prs $ \(owner, repo, prid) -> do
            cs <- GitHub.executeRequest auth $
                GitHub.pullRequestReviewsR owner repo prid GitHub.FetchAll
            cs `shouldSatisfy` isRight
  where
    prs =
      [("phadej", "github", Id 268)]
