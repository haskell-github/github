{-# LANGUAGE OverloadedStrings #-}
module GitHub.PublicSSHKeysSpec where

import GitHub
       (Auth (..), FetchCount (..), PublicSSHKey (..),github)
import GitHub.Endpoints.Users.PublicSSHKeys
       (publicSSHKeyR, publicSSHKeysR, publicSSHKeysForR)

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
  describe "publicSSHKeysFor'" $ do
    it "works" $ withAuth $ \auth -> do
      keys <- github auth publicSSHKeysForR "phadej" FetchAll
      V.length (fromRightS keys) `shouldSatisfy` (> 1)

  describe "publicSSHKeys' and publicSSHKey'" $ do
    it "works" $ withAuth $ \auth -> do
      keys <- github auth publicSSHKeysR
      V.length (fromRightS keys) `shouldSatisfy` (> 1)

      key <- github auth publicSSHKeyR (publicSSHKeyId $ V.head (fromRightS keys))
      key `shouldSatisfy` isRight
