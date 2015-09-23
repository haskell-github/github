{-# LANGUAGE OverloadedStrings #-}

module ListTeamsCurrent where

import qualified Github.Auth        as Github
import qualified Github.Teams       as Github
import           System.Environment (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [token] -> Github.listTeamsCurrent' (Github.GithubOAuth token)
              _       -> error "usage: ListTeamsCurrent <token>"
  case result of
    Left err    -> putStrLn $ "Error: " ++ show err
    Right teams -> mapM_ (putStrLn . show) teams
