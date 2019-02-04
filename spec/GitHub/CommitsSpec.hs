{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.CommitsSpec where

import qualified GitHub

import GitHub.Auth                    (Auth (..))
import GitHub.Endpoints.Repos.Commits (commitSha, commitsFor',
                                       commitsForR, diffR, mkCommitName)
import GitHub.Request                 (executeRequest)

import Control.Monad      (forM_)
import Data.Either.Compat (isRight)
import Data.List          (nub, sort)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe,
                           shouldSatisfy)

import qualified Data.Vector as V

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
  describe "commitsFor" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  commitsFor' (Just auth) "phadej" "github"
      cs `shouldSatisfy` isRight
      V.length (fromRightS cs) `shouldSatisfy` (> 300)

    -- Page size is 30, so we get 60 commits
    it "limits the response" $ withAuth $ \auth -> do
      cs <- executeRequest auth $ commitsForR "phadej" "github" (GitHub.FetchAtLeast 40)
      cs `shouldSatisfy` isRight
      let cs' = fromRightS cs
      V.length cs' `shouldSatisfy` (< 70)
      let hashes = sort $ map commitSha $ V.toList cs'
      hashes `shouldBe` nub hashes

  describe "diff" $ do
    it "works" $ withAuth $ \auth -> do
      cs <- executeRequest auth $ commitsForR "phadej" "github" (GitHub.FetchAtLeast 30)
      cs `shouldSatisfy` isRight
      let commits = take 10 . V.toList . fromRightS $ cs
      let pairs = zip commits $ drop 1 commits
      forM_ pairs $ \(a, b) -> do
        d <- executeRequest auth $ diffR "phadej" "github" (commitSha a) (commitSha b)
        d `shouldSatisfy` isRight

    it "issue #155" $ withAuth $ \auth -> do
      d <- executeRequest auth $ diffR "nomeata" "codespeed" (mkCommitName "ghc") (mkCommitName "tobami:master")
      d `shouldSatisfy` isRight

    -- diff that includes a commit where a submodule is removed
    it "issue #339" $ withAuth $ \auth -> do
      d <- executeRequest auth $ diffR "scott-fleischman" "repo-remove-submodule" "d03c152482169d809be9b1eab71dcf64d7405f76" "42cfd732b20cd093534f246e630b309186eb485d"
      d `shouldSatisfy` isRight
