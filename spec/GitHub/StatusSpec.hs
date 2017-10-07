{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.StatusSpec where

import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.FileEmbed     (embedFile)
import qualified GitHub

import GitHub.Data                      (NewStatus(..), Status(..), StatusState(..))
import GitHub.Auth                        (Auth (..))
import GitHub.Endpoints.Repos.Status
import GitHub.Request                     (executeRequest)

import Data.Either.Compat (isRight)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy, shouldBe)

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
  describe "listStatusesR" $ do
    it "decodes status json" $ do
      let statusInfo = eitherDecodeStrict $(embedFile "fixtures/status.json")
      statusState (fromRightS statusInfo) `shouldBe` Success
    
    it "works" $ withAuth $ \auth -> do
      cs <-
        executeRequest auth $
          listStatusesR
            "nixos"
            "nixpkgs"
            "426740205c62bb901d70209f2f0ce1a83864afdd"
            GitHub.FetchAll
      cs `shouldSatisfy` isRight
      V.length (fromRightS cs) `shouldSatisfy` (== 5)
  

  describe "createStatusR" $ do
    it "works" $ withAuth $ \auth -> do
      let newStatus = NewStatus { newStatusState = Pending
                                , newStatusTargetUrl = Just (GitHub.URL "https://example.com")
                                , newStatusDescription = Just "Description"
                                , newStatusContext = "cabal test"
                                }
      cs <-
        executeRequest auth $
          createStatusR
            "ocharles"
            "github"
            "71f5b2ae1bb9fbb47d7540fe8c05cb12cc53e033"
            newStatus
      case cs of
        Left e -> fail (show e)
        Right status -> do
          statusState status `shouldBe` newStatusState newStatus
          statusDescription status `shouldBe` newStatusDescription newStatus
          statusTargetUrl status `shouldBe` newStatusTargetUrl newStatus
          statusContext status `shouldBe` newStatusContext newStatus
