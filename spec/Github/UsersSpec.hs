{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Github.UsersSpec where

import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.Either.Compat (isRight)
import Data.FileEmbed     (embedFile)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe,
                           shouldSatisfy)

import Github.Auth             (GithubAuth (..))
import Github.Data.Definitions (DetailedOwner (..))
import Github.Users            (userInfoCurrent', userInfoFor')

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
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "returns information about the user" $ withAuth $ \auth -> do
      userInfo <- userInfoFor' (Just auth) "mike-burns"
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

  describe "userInfoCurrent'" $ do
    it "returns information about the autenticated user" $ withAuth $ \auth -> do
      userInfo <- userInfoCurrent' (Just auth)
      userInfo `shouldSatisfy` isRight
