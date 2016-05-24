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
              [token, team_id, username] ->
                GitHub.deleteTeamMembershipFor'
                    (GitHub.OAuth $ fromString token)
                    (GitHub.mkTeamId $ read team_id)
                    (GitHub.mkOwnerName $ fromString username)
              _                          ->
                error "usage: DeleteTeamMembershipFor <token> <team_id> <username>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
