{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [api_endpoint, token, org_login, org_admin, org_profile_name] ->
                GitHub.github
                  (GitHub.EnterpriseOAuth
                    (fromString api_endpoint)
                    (fromString token)
                  )
                  GitHub.createOrganizationR
                  (GitHub.CreateOrganization
                    (GitHub.mkOrganizationName $ fromString org_login)
                    (GitHub.mkUserName $ fromString org_admin)
                    (Just $ fromString org_profile_name)
                  )
              _                                 ->
                error "usage: CreateOrganization <api_endpoint> <token> <org_login> <org_admin> <org_profile_name>"
  case result of
    Left err  -> putStrLn $ "Error: " <> tshow err
    Right org -> putStrLn $ tshow org
