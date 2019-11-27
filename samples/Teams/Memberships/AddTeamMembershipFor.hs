{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common

import qualified GitHub

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    [token, team_id, username] -> GitHub.github
      (GitHub.OAuth $ fromString token)
      GitHub.addTeamMembershipForR
      (GitHub.mkTeamId $ read team_id)
      (GitHub.mkOwnerName $ fromString username)
      GitHub.RoleMember
    _ -> fail "usage: AddTeamMembershipFor <token> <team_id> <username>"
  case result of
    Left err   -> putStrLn $ "Error: " <> tshow err
    Right team -> putStrLn $ tshow team
