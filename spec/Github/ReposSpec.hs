{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Github.ReposSpec where

import Github.Auth          (GithubAuth (..))
import Github.Repos (currentUserRepos, userRepos', RepoPublicity(..))

-- import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.Either.Compat (isRight)
-- import Data.FileEmbed     (embedFile)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

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
  describe "currentUserRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  currentUserRepos auth All
      cs `shouldSatisfy` isRight

  describe "userRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  userRepos' (Just auth) "phadej" All
      cs `shouldSatisfy` isRight
