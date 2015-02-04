module Github.UsersSpec where

import Github.Users (userInfoFor)
import Github.Data.Definitions (DetailedOwner(..))

import Test.Hspec (it, describe, shouldBe, Spec)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "Expected a Right and got a Left"

spec :: Spec
spec =
  describe "userInfoFor" $ do
    it "returns information about the user" $ do
      userInfo <- userInfoFor "mike-burns"
      detailedOwnerLogin (fromRight userInfo) `shouldBe` "mike-burns"
