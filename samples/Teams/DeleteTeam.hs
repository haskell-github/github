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
              [token, team_id] -> GitHub.deleteTeam' (GitHub.OAuth $ fromString token) (GitHub.mkTeamId $ read team_id)
              _                -> error "usage: DeleteTeam <token> <team_id>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
