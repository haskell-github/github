{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, team_id, team_name, desc] ->
                Github.editTeam'
                  (Github.GithubOAuth token)
                  (read team_id)
                  (Github.EditTeam (Github.mkName (Proxy :: Proxy Github.Team) $ fromString team_name) (Just $ fromString desc) Github.PermissionPull)
              _                                 ->
                error "usage: EditTeam <token> <team_id> <team_name> <description>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
