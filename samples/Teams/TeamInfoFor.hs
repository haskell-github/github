{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [team_id, token] -> Github.teamInfoFor' (Just $ Github.GithubOAuth token) (read team_id)
              [team_id]        -> Github.teamInfoFor (read team_id)
              _                -> error "usage: TeamInfoFor <team_id> [auth token]"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
