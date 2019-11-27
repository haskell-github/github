{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub as GH
import Data.Text (Text)

main :: IO ()
main = do
  let auth = GH.OAuth "auth_token"
  eDeployKey <- GH.github auth GH.createRepoDeployKeyR "your_owner" "your_repo" newDeployKey
  case eDeployKey of
    Left err        -> putStrLn $ "Error: " ++ show err
    Right deployKey -> print deployKey

newDeployKey :: GH.NewRepoDeployKey
newDeployKey = GH.NewRepoDeployKey publicKey "test-key" True
  where
    publicKey :: Text
    publicKey = "your_public_key"
