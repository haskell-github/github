{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub as GH

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token] -> GH.github (GH.OAuth $ fromString token) GH.listTeamsCurrentR GH.FetchAll
              _       -> error "usage: ListTeamsCurrent <token>"
  case result of
    Left err    -> putStrLn $ "Error: " <> tshow err
    Right teams -> mapM_ (putStrLn . tshow) teams
