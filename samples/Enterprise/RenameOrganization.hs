{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [api_endpoint, token, current_name, new_name] ->
                GitHub.github
                  (GitHub.EnterpriseOAuth
                    (fromString api_endpoint)
                    (fromString token)
                  )
                  GitHub.renameOrganizationR
                  (GitHub.mkOrganizationName $ fromString current_name)
                  (GitHub.RenameOrganization
                    (GitHub.mkOrganizationName $ fromString new_name)
                  )
              _                                 ->
                error "usage: RenameOrganization <api_endpoint> <token> <current_name> <new_name>"
  case result of
    Left err -> putStrLn $ "Error: " <> tshow err
    Right x  -> putStrLn $ tshow x
