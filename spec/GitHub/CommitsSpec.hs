{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.CommitsSpec where

import GitHub.Auth          (GithubAuth (..))
import GitHub.Endpoints.Repos.Commits (Commit, mkName, commitSha, commitsFor', commitsForR, diffR)
import GitHub.Request       (executeRequest)

import Control.Monad      (forM_)
import Data.Either.Compat (isRight)
import Data.List          (sort, nub)
import Data.Proxy         (Proxy (..))
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

import qualified Data.Vector as V

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
  describe "commitsFor" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  commitsFor' (Just auth) "phadej" "github"
      cs `shouldSatisfy` isRight
      V.length (fromRightS cs) `shouldSatisfy` (> 300)

    -- Page size is 30, so we get 60 commits
    it "limits the response" $ withAuth $ \auth -> do
      cs <- executeRequest auth $ commitsForR "phadej" "github" (Just 40)
      cs `shouldSatisfy` isRight
      let cs' = fromRightS cs
      V.length cs' `shouldSatisfy` (< 70)
      let hashes = sort $ map commitSha $ V.toList cs'
      hashes `shouldBe` nub hashes

  describe "diff" $ do
    it "works" $ withAuth $ \auth -> do
      cs <- executeRequest auth $ commitsForR "phadej" "github" (Just 30)
      cs `shouldSatisfy` isRight
      let commits = take 10 . V.toList . fromRightS $ cs
      let pairs = zip commits $ drop 1 commits
      forM_ pairs $ \(a, b) -> do
        d <- executeRequest auth $ diffR "phadej" "github" (commitSha a) (commitSha b)
        d `shouldSatisfy` isRight

    it "issue #155" $ withAuth $ \auth -> do
      let mkCommitName = mkName (Proxy :: Proxy Commit)
      d <- executeRequest auth $ diffR "nomeata" "codespeed" (mkCommitName "ghc") (mkCommitName "tobami:master")
      d `shouldSatisfy` isRight
