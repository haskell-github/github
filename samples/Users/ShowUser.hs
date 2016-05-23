{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Data.Maybe (fromMaybe)

import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub

main :: IO ()
main = do
  auth <- getAuth
  possibleUser <- GitHub.userInfoFor' auth "mike-burns"
  putStrLn $ either (("Error: " <>) . tshow) formatUser possibleUser

formatUser :: GitHub.User -> Text
formatUser user =
  (formatName userName login) <> "\t" <> (fromMaybe "" company) <> "\t" <>
    (fromMaybe "" location) <> "\n" <>
    (fromMaybe "" blog) <> "\t" <> "<" <> (fromMaybe "" email) <> ">" <> "\n" <>
    GitHub.getUrl htmlUrl <> "\t" <> tshow createdAt <> "\n" <>
    "hireable: " <> formatHireable (fromMaybe False isHireable) <> "\n\n" <>
    (fromMaybe "" bio)
  where
    userName = GitHub.userName user
    login = GitHub.userLogin user
    company = GitHub.userCompany user
    location = GitHub.userLocation user
    blog = GitHub.userBlog user
    email = GitHub.userEmail user
    htmlUrl = GitHub.userHtmlUrl user
    createdAt = GitHub.userCreatedAt user
    isHireable = GitHub.userHireable user
    bio = GitHub.userBio user

formatName :: Maybe Text -> GitHub.Name GitHub.User -> Text
formatName Nothing login = GitHub.untagName login
formatName (Just name) login = name <> "(" <> GitHub.untagName login <> ")"

formatHireable :: Bool -> Text
formatHireable True = "yes"
formatHireable False = "no"
