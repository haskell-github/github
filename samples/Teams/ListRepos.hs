{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified GitHub as GH

main :: IO ()
main = do
    args <- getArgs
    possibleRepos <- case args of
      [team_id, token] -> GH.github (GH.OAuth $ fromString token) GH.listTeamReposR (GH.mkTeamId $ read team_id)
      [team_id]        -> GH.github' GH.listTeamReposR (GH.mkTeamId $ read team_id)
      _                -> error "usage: TeamListRepos <team_id> [auth token]"
    case possibleRepos of
      Left err    -> putStrLn $ "Error: " <> tshow err
      Right repos -> putStrLn $ tshow repos
