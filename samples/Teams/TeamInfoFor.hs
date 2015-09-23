{-# LANGUAGE OverloadedStrings #-}

module TeamInfoFor where

import qualified Github.Auth        as Github
import qualified Github.Teams       as Github
import           System.Environment (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [team_id, token] -> Github.teamInfoFor' (Just $ Github.GithubOAuth token) (read team_id)
              [team_id]        -> Github.teamInfoFor (read team_id)
              _                -> error "usage: TeamInfoFor <team_id> [auth token]"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
