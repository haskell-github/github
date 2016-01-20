{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import qualified Github.Request         as Github
import qualified Github.Users.Followers as Github

main :: IO ()
main = do
    auth <- getAuth
    possibleUsers <- Github.executeRequestMaybe auth $ Github.usersFollowedByR "mike-burns" Nothing
    putStrLn $ either (("Error: " <>) . tshow)
                      (foldMap ((<> "\n") . formatUser))
                      possibleUsers

formatUser :: Github.SimpleUser -> Text
formatUser = Github.untagName . Github.simpleUserLogin

