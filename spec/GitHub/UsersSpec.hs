{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.UsersSpec where

import Data.Aeson         (eitherDecodeStrict)
import Data.Either.Compat (isLeft, isRight)
import Data.FileEmbed     (embedFile)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec
       (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

import qualified GitHub

import GitHub.Data
       (Auth (..), Organization (..), User (..), fromOwner)
import GitHub.Endpoints.Users
       (ownerInfoForR, userInfoCurrentR, userInfoForR)
import GitHub.Endpoints.Users.Followers (usersFollowedByR, usersFollowingR)
import GitHub.Request                   (github)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

fromLeftS :: Show b => Either a b -> a
fromLeftS (Left b) = b
fromLeftS (Right a) = error $ "Expected a Left and got a RIght" ++ show a

withAuth :: (Auth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (OAuth $ fromString token)

spec :: Spec
spec = do
  describe "userInfoFor" $ do
    it "decodes user json" $ do
      let userInfo = eitherDecodeStrict $(embedFile "fixtures/user.json")
      userLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "decodes user-bot json" $ do
      let userInfo = eitherDecodeStrict $(embedFile "fixtures/user-bot.json")
      userLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "returns information about the user" $ withAuth $ \auth -> do
      userInfo <- github auth userInfoForR "mike-burns"
      userLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "catches http exceptions" $ withAuth $ \auth -> do
      userInfo <- github auth userInfoForR "i-hope-this-user-will-never-exist"
      userInfo `shouldSatisfy` isLeft

    it "should fail for organization" $ withAuth $ \auth -> do
      userInfo <- github auth userInfoForR "haskell"
      userInfo `shouldSatisfy` isLeft

  describe "ownerInfoFor" $ do
    it "works for users and organizations" $ withAuth $ \auth -> do
      a <- github auth ownerInfoForR "haskell"
      b <- github auth ownerInfoForR "phadej"
      a `shouldSatisfy` isRight
      b `shouldSatisfy` isRight
      (organizationLogin . fromRightS . fromOwner . fromRightS $ a) `shouldBe` "haskell"
      (userLogin . fromLeftS . fromOwner . fromRightS $ b) `shouldBe` "phadej"

  describe "userInfoCurrentR" $ do
    it "returns information about the authenticated user" $ withAuth $ \auth -> do
      userInfo <- github auth userInfoCurrentR
      userInfo `shouldSatisfy` isRight

  describe "usersFollowing" $ do
    it "works" $ withAuth $ \auth -> do
      us <- github auth usersFollowingR "phadej" (GitHub.FetchAtLeast 10)
      us `shouldSatisfy` isRight

  describe "usersFollowedBy" $ do
    it "works" $ withAuth $ \auth -> do
      us <- github auth usersFollowedByR "phadej" (GitHub.FetchAtLeast 10)
      us `shouldSatisfy` isRight
