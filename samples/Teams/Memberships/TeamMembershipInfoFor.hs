{-# LANGUAGE OverloadedStrings #-}

module TeamMembershipInfoFor where

import qualified Github.Auth              as Github
import qualified Github.Teams.Memberships as Github
import           System.Environment       (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [team_id, username, token] ->
                Github.teamMembershipInfoFor' (Just $ Github.GithubOAuth token) (read team_id) username
              [team_id, username]        ->
                Github.teamMembershipInfoFor (read team_id) username
              _                          ->
                error "usage: TeamMembershipInfoFor <team_id> <username> [token]"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
