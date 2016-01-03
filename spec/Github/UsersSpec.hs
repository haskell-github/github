{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Github.UsersSpec where

import Github.Auth (GithubAuth(..))
import Github.Users (userInfoFor')
import Github.Data.Definitions (DetailedOwner(..))

import Data.Aeson.Compat (eitherDecodeStrict)
import Test.Hspec (it, describe, shouldBe, pendingWith, Spec)
import System.Environment (lookupEnv)
import Data.FileEmbed (embedFile)

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
spec =
  describe "userInfoFor" $ do
    it "decodes user json" $ do
      let userInfo = eitherDecodeStrict $(embedFile "fixtures/user.json") :: Either String DetailedOwner
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"

    it "returns information about the user" $ withAuth $ \auth -> do
      userInfo <- userInfoFor' (Just auth) "mike-burns"
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"
