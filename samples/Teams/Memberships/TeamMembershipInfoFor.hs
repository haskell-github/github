{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [team_id, username, token] ->
                Github.teamMembershipInfoFor' (Just $ Github.GithubOAuth token) (Github.mkTeamId $ read team_id) (Github.mkOwnerName $ fromString username)
              [team_id, username]        ->
                Github.teamMembershipInfoFor (Github.mkTeamId $ read team_id) (Github.mkOwnerName $ fromString username)
              _                          ->
                error "usage: TeamMembershipInfoFor <team_id> <username> [token]"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
