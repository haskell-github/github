module Github.UsersSpec where

import Github.Users (userInfoFor)
import Github.Data.Definitions (DetailedOwner(..))

import Test.Hspec (it, describe, shouldBe, Spec)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec =
  describe "userInfoFor" $ do
    it "returns information about the user" $ do
      userInfo <- userInfoFor "mike-burns"
      detailedOwnerLogin (fromRightS userInfo) `shouldBe` "mike-burns"
