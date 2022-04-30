{-# LANGUAGE OverloadedStrings #-}
module GitHub.ReposSpec where

import GitHub
       (Auth (..), FetchCount (..), Repo (..), RepoPublicity (..), github,
       repositoryR)
import GitHub.Endpoints.Repos (currentUserReposR, languagesForR, userReposR)

import Data.Either.Compat (isRight)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec
       (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

import qualified Data.HashMap.Strict as HM

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
  describe "repositoryR" $ do
    it "works" $ withAuth $ \auth -> do
      er <- github auth repositoryR "haskell-github" "github"
      er `shouldSatisfy` isRight
      let Right r = er
      -- https://github.com/haskell-github/github/pull/219
      repoDefaultBranch r `shouldBe` Just "master"

  describe "currentUserRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <- github auth currentUserReposR RepoPublicityAll FetchAll
      cs `shouldSatisfy` isRight

  describe "userRepos" $ do
    it "works" $ withAuth $ \auth -> do
      cs <- github auth userReposR "phadej" RepoPublicityAll FetchAll
      cs `shouldSatisfy` isRight

  describe "languagesFor'" $ do
    it "works" $ withAuth $ \auth -> do
      ls <- github auth languagesForR "haskell-github" "github"
      ls `shouldSatisfy` isRight
      fromRightS ls `shouldSatisfy` HM.member "Haskell"
