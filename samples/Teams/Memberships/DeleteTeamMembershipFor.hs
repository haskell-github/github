{-# LANGUAGE OverloadedStrings #-}

module DeleteTeamMembershipFor where

import qualified Github.Auth              as Github
import qualified Github.Teams.Memberships as Github
import           System.Environment       (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token, team_id, username] ->
                Github.deleteTeamMembershipFor' (Github.GithubOAuth token) (read team_id) username
              _                          ->
                error "usage: DeleteTeamMembershipFor <token> <team_id> <username>"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
