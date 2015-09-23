{-# LANGUAGE OverloadedStrings #-}

module EditTeam where

import qualified Github.Auth        as Github
import qualified Github.Teams       as Github
import           System.Environment (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token, team_id, team_name, desc] ->
                Github.editTeam'
                  (Github.GithubOAuth token)
                  (read team_id)
                  (Github.EditTeam team_name (Just desc) Github.PermissionPull)
              _                                 ->
                error "usage: EditTeam <token> <team_id> <team_name> <description>"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
