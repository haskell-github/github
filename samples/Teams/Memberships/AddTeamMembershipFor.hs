{-# LANGUAGE OverloadedStrings #-}

module AddTeamMembershipFor where

import qualified Github.Auth              as Github
import qualified Github.Teams.Memberships as Github
import           System.Environment       (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token, team_id, username] ->
                Github.addTeamMembershipFor' (Github.GithubOAuth token) (read team_id) username Github.RoleMember
              _                          ->
                error "usage: AddTeamMembershipFor <token> <team_id> <username>"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
