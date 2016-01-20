{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Data.Maybe (fromMaybe)

import qualified Github.Users as Github

main :: IO ()
main = do
  auth <- getAuth
  possibleUser <- Github.userInfoFor' auth "mike-burns"
  putStrLn $ either (("Error: " <>) . tshow) formatUser possibleUser

formatUser :: Github.User -> Text
formatUser user =
  (formatName userName login) <> "\t" <> (fromMaybe "" company) <> "\t" <>
    (fromMaybe "" location) <> "\n" <>
    (fromMaybe "" blog) <> "\t" <> "<" <> (fromMaybe "" email) <> ">" <> "\n" <>
    htmlUrl <> "\t" <> tshow createdAt <> "\n" <>
    "hireable: " <> formatHireable (fromMaybe False isHireable) <> "\n\n" <>
    (fromMaybe "" bio)
  where
    userName = Github.userName user
    login = Github.userLogin user
    company = Github.userCompany user
    location = Github.userLocation user
    blog = Github.userBlog user
    email = Github.userEmail user
    htmlUrl = Github.userHtmlUrl user
    createdAt = Github.userCreatedAt user
    isHireable = Github.userHireable user
    bio = Github.userBio user

formatName :: Maybe Text -> Github.Name Github.User -> Text
formatName Nothing login = Github.untagName login
formatName (Just name) login = name <> "(" <> Github.untagName login <> ")"

formatHireable :: Bool -> Text
formatHireable True = "yes"
formatHireable False = "no"
