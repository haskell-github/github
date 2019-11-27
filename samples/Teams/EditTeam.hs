{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, team_id, team_name, desc] ->
                GitHub.github
                  (GitHub.OAuth $ fromString token)
                  GitHub.editTeamR
                  (GitHub.mkTeamId $ read team_id)
                  (GitHub.EditTeam (GitHub.mkTeamName $ fromString team_name) (Just $ fromString desc) GitHub.PermissionPull)
              _                                 ->
                error "usage: EditTeam <token> <team_id> <team_name> <description>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
