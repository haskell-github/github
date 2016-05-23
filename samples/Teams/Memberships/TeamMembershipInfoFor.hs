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
              [team_id, username, token] ->
                GitHub.teamMembershipInfoFor' (Just $ GitHub.OAuth $ fromString token) (GitHub.mkTeamId $ read team_id) (GitHub.mkOwnerName $ fromString username)
              [team_id, username]        ->
                GitHub.teamMembershipInfoFor (GitHub.mkTeamId $ read team_id) (GitHub.mkOwnerName $ fromString username)
              _                          ->
                error "usage: TeamMembershipInfoFor <team_id> <username> [token]"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
