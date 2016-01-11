{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Organizations.Teams as Github

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token] -> Github.listTeamsCurrent' (Github.GithubOAuth token)
              _       -> error "usage: ListTeamsCurrent <token>"
  case result of
    Left err    -> putStrLn $ "Error: " <> tshow err
    Right teams -> mapM_ (putStrLn . tshow) teams
