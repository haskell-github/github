{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, team_id, username] ->
                Github.deleteTeamMembershipFor'
                    (Github.GithubOAuth token)
                    (Github.mkTeamId $ read team_id)
                    (Github.mkOwnerName $ fromString username)
              _                          ->
                error "usage: DeleteTeamMembershipFor <token> <team_id> <username>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
