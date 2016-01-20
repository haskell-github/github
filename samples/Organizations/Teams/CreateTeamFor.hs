{-# LANGUAGE OverloadedStrings #-}

module CreateTeamFor where

import qualified Github.Auth        as Github
import qualified Github.Teams       as Github
import           System.Environment (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token, org, team, desc, repos] ->
                Github.createTeamFor'
                  (Github.OAuth token)
                  org
                  (Github.CreateTeam team (Just desc) (read repos :: [String]) Github.PermissionPull)
              _                               ->
                error "usage: CreateTeamFor <token> <org> <team_name> <description> <[\"repos\"]>"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
