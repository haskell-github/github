{-# LANGUAGE OverloadedStrings #-}

module ListTeamsForOrganization where

import qualified Github.Auth                as Github
import qualified Github.Organizations.Teams as Github
import           System.Environment         (getArgs)

main = do
  args <- getArgs
  result <- case args of
              [team, token] -> Github.teamsOf' (Just $ Github.OAuth token) team
              [team]        -> Github.teamsOf team
              _             -> error "usage: ListTeamsForOrganization <team> [auth token]"
  case result of
    Left err    -> putStrLn $ "Error: " ++ show err
    Right teams -> mapM_ (putStrLn . show) teams
