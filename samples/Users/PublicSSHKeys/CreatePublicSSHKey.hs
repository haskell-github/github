{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub.Data.PublicSSHKeys as PK
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import qualified GitHub.Auth as Auth

main :: IO ()
main = do
  let auth = Auth.OAuth "auth_token"
  ePublicSSHKey <- PK.createUserPublicSSHKey' auth newPublicSSHKey
  case ePublicSSHKey of
    Left err -> putStrLn $ "Error: " ++ show err
    Right publicSSHKey -> print publicSSHKey

newPublicSSHKey :: PK.NewPublicSSHKey
newPublicSSHKey =
    PK.NewPublicSSHKey
        { PK.newPublicSSHKeyKey = "test-key"
        , PK.newPublicSSHKeyTitle = "some-name-for-your-key"
        }
