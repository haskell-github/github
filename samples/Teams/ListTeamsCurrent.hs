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
              [token] -> GitHub.listTeamsCurrent' (GitHub.OAuth $ fromString token)
              _       -> error "usage: ListTeamsCurrent <token>"
  case result of
    Left err    -> putStrLn $ "Error: " <> tshow err
    Right teams -> mapM_ (putStrLn . tshow) teams
