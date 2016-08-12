{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GitHub.Data.Id (Id (..))
import qualified GitHub.Endpoints.Repos.DeployKeys as DK
import qualified GitHub.Auth as Auth

main :: IO ()
main = do
  let auth = Auth.OAuth "auth_token"
  eDeployKey <- DK.deleteRepoDeployKey' auth "your_owner" "your_repo" (Id 18530161)
  case eDeployKey of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right _) -> putStrLn $ "Deleted deploy key!"
