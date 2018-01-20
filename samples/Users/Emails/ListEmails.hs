{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()
import qualified GitHub.Endpoints.Users.Emails as GitHub


main :: IO ()
main = do
    emails <- GitHub.currentUserEmails' (GitHub.OAuth "token")
    putStrLn $ either (("Error: " <>) . tshow)
                      (foldMap ((<> "\n") . formatEmail))
                      emails

formatEmail :: GitHub.Email -> Text
formatEmail e = GitHub.emailAddress e <> if GitHub.emailPrimary e then " [primary]" else ""
