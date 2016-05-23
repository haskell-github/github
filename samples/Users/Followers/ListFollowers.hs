{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified GitHub

main :: IO ()
main = do
    auth <- getAuth
    possibleUsers <- GitHub.executeRequestMaybe auth $ GitHub.usersFollowingR "mike-burns" GitHub.FetchAll 
    putStrLn $ either (("Error: " <>) . tshow)
                      (foldMap ((<> "\n") . formatUser))
                      possibleUsers

formatUser :: GitHub.SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin
