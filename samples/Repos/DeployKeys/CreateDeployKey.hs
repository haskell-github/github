{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub.Data.DeployKeys as DK
import qualified GitHub.Endpoints.Repos.DeployKeys as DK
import qualified GitHub.Auth as Auth
import Data.Text (Text)

main :: IO ()
main = do
  let auth = Auth.OAuth "auth_token"
  eDeployKey <- DK.createRepoDeployKey' auth "your_owner" "your_repo" newDeployKey
  case eDeployKey of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right deployKey) -> putStrLn $ show deployKey

newDeployKey :: DK.NewRepoDeployKey
newDeployKey = DK.NewRepoDeployKey publicKey "test-key" True
  where
    publicKey :: Text
    publicKey = "your_public_key"
