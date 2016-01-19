{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Github.UsersSpec where

import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.Either.Compat (isRight, isLeft)
import Data.FileEmbed     (embedFile)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe,
                           shouldSatisfy)

import Github.Auth             (GithubAuth (..))
import Github.Data.Definitions (GithubOwner (..))
import Github.Request          (executeRequest)
import Github.Users            (userInfoCurrent', userInfoFor')
import Github.Users.Followers  (usersFollowedByR, usersFollowingR)

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
  describe "userInfoFor" $ do
    it "decodes user json" $ do
      let userInfo = eitherDecodeStrict $(embedFile "fixtures/user.json")
      githubOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "returns information about the user" $ withAuth $ \auth -> do
      userInfo <- userInfoFor' (Just auth) "mike-burns"
      githubOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "catches http exceptions" $ withAuth $ \auth -> do
      userInfo <- userInfoFor' (Just auth) "i-hope-this-user-will-never-exist"
      userInfo `shouldSatisfy` isLeft

  describe "userInfoCurrent'" $ do
    it "returns information about the autenticated user" $ withAuth $ \auth -> do
      userInfo <- userInfoCurrent' auth
      userInfo `shouldSatisfy` isRight

  describe "usersFollowing" $ do
    it "works" $ withAuth $ \auth -> do
      us <- executeRequest auth $ usersFollowingR "phadej" (Just 10)
      us `shouldSatisfy` isRight

  describe "usersFollowedBy" $ do
    it "works" $ withAuth $ \auth -> do
      us <- executeRequest auth $ usersFollowedByR "phadej" (Just 10)
      us `shouldSatisfy` isRight
