{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.ReposSpec where

import GitHub.Auth          (Auth (..))
import GitHub.Endpoints.Repos (currentUserRepos, userRepos', RepoPublicity(..))

import Data.Either.Compat (isRight)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (Auth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (OAuth token)

spec :: Spec
spec = do
  describe "currentUserRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  currentUserRepos auth RepoPublicityAll
      cs `shouldSatisfy` isRight

  describe "userRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  userRepos' (Just auth) "phadej" RepoPublicityAll
      cs `shouldSatisfy` isRight
