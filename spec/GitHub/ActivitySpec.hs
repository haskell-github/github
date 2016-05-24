{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.ActivitySpec where

import qualified GitHub

import GitHub.Auth                        (Auth (..))
import GitHub.Endpoints.Activity.Starring (myStarredAcceptStarR)
import GitHub.Endpoints.Activity.Watching (watchersForR)
import GitHub.Request                     (executeRequest)

import Data.Either.Compat (isRight)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

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
  describe "watchersForR" $ do
    it "works" $ withAuth $ \auth -> do
      cs <- executeRequest auth $ watchersForR "phadej" "github" GitHub.FetchAll 
      cs `shouldSatisfy` isRight
      V.length (fromRightS cs) `shouldSatisfy` (> 10)
  describe "myStarredR" $ do
      it "works" $ withAuth $ \auth -> do
          cs <- executeRequest auth $ myStarredAcceptStarR (GitHub.FetchAtLeast 31)
          cs `shouldSatisfy` isRight
          fromRightS cs `shouldSatisfy` (\xs -> V.length xs > 30)
