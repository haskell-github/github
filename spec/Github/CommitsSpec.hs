{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Github.CommitsSpec where

import Github.Auth                  (GithubAuth (..))
import Github.Repos.Commits (commitsFor')

-- import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.Either.Compat (isRight)
-- import Data.FileEmbed     (embedFile)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith,
                           shouldSatisfy)

import qualified Data.Vector as V

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
  describe "commitsFor" $ do
    it "works" $ withAuth $ \auth -> do
      cs <-  commitsFor' (Just auth) "phadej" "github"
      V.length (fromRightS cs) `shouldSatisfy` (> 300)
      cs `shouldSatisfy` isRight
