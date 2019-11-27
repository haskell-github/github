{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub as GH

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [team_id, token] -> GH.github (GH.OAuth $ fromString token) GH.teamInfoForR (GH.mkTeamId $ read team_id)
              [team_id]        -> GH.github' GH.teamInfoForR (GH.mkTeamId $ read team_id)
              _                -> error "usage: TeamInfoFor <team_id> [auth token]"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
