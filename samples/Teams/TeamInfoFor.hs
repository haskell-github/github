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
              [team_id, token] -> GitHub.teamInfoFor' (Just $ GitHub.OAuth $ fromString token) (GitHub.mkTeamId $ read team_id)
              [team_id]        -> GitHub.teamInfoFor (GitHub.mkTeamId $ read team_id)
              _                -> error "usage: TeamInfoFor <team_id> [auth token]"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
