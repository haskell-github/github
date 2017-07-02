{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.ReleasesSpec where

import qualified GitHub

import GitHub.Auth                     (Auth (..))
import GitHub.Endpoints.Repos.Releases
       (Release (..), latestReleaseR, releaseByTagNameR, releaseR, releasesR)
import GitHub.Request                  (executeRequest)

import Data.Either.Compat (isRight)
import Data.Proxy         (Proxy (..))
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec
       (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

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
  let v154Id   = GitHub.mkId (Proxy :: Proxy Release) 5254449
      v154Text = "v1.5.4"
  describe "releasesR" $ do
    it "works" $ withAuth $ \auth -> do
      rs <- executeRequest auth $ releasesR "calleerlandsson" "pick" GitHub.FetchAll
      rs `shouldSatisfy` isRight
      V.length (fromRightS rs) `shouldSatisfy` (> 14)
  describe "releaseR" $ do
    it "works" $ withAuth $ \auth -> do
      rs <- executeRequest auth $ releaseR "calleerlandsson" "pick" v154Id
      rs `shouldSatisfy` isRight
      releaseTagName (fromRightS rs)`shouldBe` v154Text
  describe "latestReleaseR" $ do
    it "works" $ withAuth $ \auth -> do
      rs <- executeRequest auth $ latestReleaseR "calleerlandsson" "pick"
      rs `shouldSatisfy` isRight
  describe "releaseByTagNameR" $ do
    it "works" $ withAuth $ \auth -> do
      rs <- executeRequest auth $ releaseByTagNameR "calleerlandsson" "pick" v154Text
      rs `shouldSatisfy` isRight
      releaseId (fromRightS rs)`shouldBe` v154Id
