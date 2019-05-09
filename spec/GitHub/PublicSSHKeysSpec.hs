{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.PublicSSHKeysSpec where

import GitHub                 (Auth (..), PublicSSHKeyBasic (..), PublicSSHKey (..),
                               executeRequest, repositoryR)
import GitHub.Endpoints.Users.PublicSSHKeys (publicSSHKeysFor', publicSSHKeys',
                               publicSSHKey')

import Data.Either.Compat (isRight)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe,
                           shouldSatisfy)

import qualified Data.HashMap.Strict as HM
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
  describe "publicSSHKeysFor'" $ do
    it "works" $ do
      keys <- publicSSHKeysFor' "phadej"
      V.length (fromRightS keys) `shouldSatisfy` (> 1)

  describe "publicSSHKeys' and publicSSHKey'" $ do
    it "works" $ withAuth $ \auth -> do
      keys <- publicSSHKeys' auth
      V.length (fromRightS keys) `shouldSatisfy` (> 1)

      key <- publicSSHKey' auth (publicSSHKeyId $ V.head (fromRightS keys))
      key `shouldSatisfy` isRight
