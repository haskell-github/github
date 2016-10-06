{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub
import qualified GitHub.Endpoints.Organizations.Teams as GitHub

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, team_id, org, repo] ->
                 GitHub.addOrUpdateTeamRepo'
                    (GitHub.OAuth $ fromString token)
                    (GitHub.mkTeamId $ read team_id)
                    (GitHub.mkOrganizationName $ fromString org)
                    (GitHub.mkRepoName $ fromString repo)
                    GitHub.PermissionPull
              _                          ->
                error "usage: AddOrUpdateTeamRepo <token> <team_id> <org> <repo>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
