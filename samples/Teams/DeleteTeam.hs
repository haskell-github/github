{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, team_id] -> Github.deleteTeam' (Github.GithubOAuth token) (read team_id)
              _                -> error "usage: DeleteTeam <token> <team_id>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
