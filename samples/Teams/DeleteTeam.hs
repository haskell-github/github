{-# LANGUAGE OverloadedStrings #-}

module DeleteTeam where

import qualified Github.Auth        as Github
import qualified Github.Teams       as Github
import           System.Environment (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token, team_id] -> Github.deleteTeam' (Github.GithubOAuth token) (read team_id)
              _                -> error "usage: DeleteTeam <token> <team_id>"
  case result of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right team -> putStrLn $ show team
