{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import qualified GitHub.Auth as Auth
import Data.List (intercalate)
import Data.Vector (toList)

main :: IO ()
main = do
  -- Fetch the SSH public keys of another user
  ePublicSSHKeys <- PK.publicSSHKeysFor' "github_name"
  case ePublicSSHKeys of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right publicSSHKeys) -> putStrLn $ intercalate "\n" $ map show (toList publicSSHKeys)

  -- Fetch my SSH public keys
  let auth = Auth.OAuth "auth_token"
  eMyPublicSSHKeys <- PK.publicSSHKeys' auth
  case eMyPublicSSHKeys of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right publicSSHKeys) -> putStrLn $ intercalate "\n" $ map show (toList publicSSHKeys)

