{-# LANGUAGE OverloadedStrings #-}
module GitHub.EventsSpec where

import Data.Either        (isRight)
import Data.String        (fromString)
import Prelude ()
import Prelude.Compat
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, shouldSatisfy,
                           pendingWith)

import qualified GitHub
import GitHub.Data (Auth(..))

fromRightS :: Show a => Either a b -> b
fromRightS (Left xs) = error $ "Should be Right" ++ show xs
fromRightS (Right xs) = xs

withAuth :: (Auth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (OAuth $ fromString token)

spec :: Spec
spec = do
  describe "repositoryEventsR" $ do
    it "returns non empty list of events" $ shouldSucceed $
      GitHub.repositoryEventsR "phadej" "github" 1
  describe "userEventsR" $ do
    it "returns non empty list of events" $ shouldSucceed $ GitHub.userEventsR "phadej" 1 
  where shouldSucceed f = withAuth $ \auth -> do
          cs <- GitHub.executeRequest auth $ f
          cs `shouldSatisfy` isRight
          length (fromRightS cs) `shouldSatisfy` (> 1)
