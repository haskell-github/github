{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub.Data.DeployKeys as DK
import qualified GitHub.Endpoints.Repos.DeployKeys as DK
import qualified GitHub.Auth as Auth
import Data.List (intercalate)
import Data.Vector (toList)

main :: IO ()
main = do
  let auth = Auth.OAuth "auth_token"
  eDeployKeys <- DK.deployKeysFor' auth "your_owner" "your_repo"
  case eDeployKeys of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right deployKeys) -> putStrLn $ intercalate "\n" $ map formatRepoDeployKey (toList deployKeys)

formatRepoDeployKey :: DK.RepoDeployKey -> String
formatRepoDeployKey = show

