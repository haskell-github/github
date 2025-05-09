{-# LANGUAGE OverloadedStrings #-}
module GitHub.IssuesSpec where

import qualified GitHub

import Prelude ()
import Prelude.Compat

import Data.Either.Compat  (isRight)
import Data.Foldable       (for_)
import Data.String         (fromString)
import Network.HTTP.Client (newManager, responseBody)
import System.Environment  (lookupEnv)
import Test.Hspec          (Spec, describe, expectationFailure, it, pendingWith, shouldSatisfy)


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
            cs <- GitHub.executeRequest auth $
                GitHub.issuesForRepoR owner repo mempty GitHub.FetchAll
            case cs of
              Left e ->
                expectationFailure . show $ e
              Right cs' -> do
                for_ cs' $ \i -> do
                  cms <- GitHub.executeRequest auth $
                    GitHub.commentsR owner repo (GitHub.issueNumber i) 1
                  cms `shouldSatisfy` isRight

    describe "issuesForRepoR paged" $ do
        it "works" $ withAuth $ \auth -> for_ repos $ \(owner, repo) -> do
            mgr <- newManager GitHub.tlsManagerSettings
            ret <- GitHub.executeRequestWithMgrAndRes mgr auth $
                GitHub.issuesForRepoR owner repo mempty (GitHub.FetchPage (GitHub.PageParams (Just 2) (Just 1)))

            case ret of
              Left e ->
                expectationFailure . show $ e
              Right res -> do
                let issues = responseBody res
                length issues `shouldSatisfy` (<= 2)

                for_ issues $ \i -> do
                  cms <- GitHub.executeRequest auth $
                    GitHub.commentsR owner repo (GitHub.issueNumber i) 1
                  cms `shouldSatisfy` isRight

    describe "issueR" $ do
        it "fetches issue #428" $ withAuth $ \auth -> do
            resIss <- GitHub.executeRequest auth $
                GitHub.issueR "haskell-github" "github" (GitHub.IssueNumber 428)
            resIss `shouldSatisfy` isRight
  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("haskell-github", "github")
      ]
