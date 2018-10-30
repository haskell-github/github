{-# LANGUAGE OverloadedStrings #-}
module GitHub.RateLimitSpec where

import qualified GitHub

import Prelude ()
import Prelude.Compat

import Data.Either.Compat (isRight)
import Data.String        (fromString)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (GitHub.Auth -> IO ()) -> IO ()
withAuth action = do
    mtoken <- lookupEnv "GITHUB_TOKEN"
    case mtoken of
        Nothing    -> pendingWith "no GITHUB_TOKEN"
        Just token -> action (GitHub.OAuth $ fromString token)

spec :: Spec
spec = describe "rateLimitR" $
    it "works" $ withAuth $ \auth -> do
        cs <- GitHub.executeRequest auth GitHub.rateLimitR
        cs `shouldSatisfy` isRight
