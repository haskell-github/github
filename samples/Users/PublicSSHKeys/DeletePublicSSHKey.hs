{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GitHub.Data.Id (Id (..))
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import qualified GitHub.Auth as Auth

main :: IO ()
main = do
  let auth = Auth.OAuth "auth_token"
  ePublicSSHKey <- PK.deleteUserPublicSSHKey' auth (Id 18530161)
  case ePublicSSHKey of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right _) -> putStrLn $ "Deleted public SSH key!"
