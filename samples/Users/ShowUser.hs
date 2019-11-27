{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Data.Maybe (fromMaybe)

import qualified GitHub as GH

main :: IO ()
main = do
  mauth <- getAuth
  possibleUser <- maybe GH.github' GH.github mauth GH.userInfoForR "mike-burns"
  putStrLn $ either (("Error: " <>) . tshow) formatUser possibleUser

formatUser :: GH.User -> Text
formatUser user =
  formatName userName login <> "\t" <> fromMaybe "" company <> "\t" <>
    fromMaybe "" location <> "\n" <>
    fromMaybe "" blog <> "\t" <> "<" <> fromMaybe "" email <> ">" <> "\n" <>
    GH.getUrl htmlUrl <> "\t" <> tshow createdAt <> "\n" <>
    "hireable: " <> formatHireable (fromMaybe False isHireable) <> "\n\n" <>
    fromMaybe "" bio
  where
    userName = GH.userName user
    login = GH.userLogin user
    company = GH.userCompany user
    location = GH.userLocation user
    blog = GH.userBlog user
    email = GH.userEmail user
    htmlUrl = GH.userHtmlUrl user
    createdAt = GH.userCreatedAt user
    isHireable = GH.userHireable user
    bio = GH.userBio user

formatName :: Maybe Text -> GH.Name GH.User -> Text
formatName Nothing login = GH.untagName login
formatName (Just name) login = name <> "(" <> GH.untagName login <> ")"

formatHireable :: Bool -> Text
formatHireable True = "yes"
formatHireable False = "no"
