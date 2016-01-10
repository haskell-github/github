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

formatUser :: Github.GithubOwner -> Text
formatUser user@(Github.GithubOrganization {}) =
  "Organization: " <> (formatName userName login) <> "\t" <>
    (fromMaybe "" company) <> "\t" <>
    (fromMaybe "" location) <> "\n" <>
    (fromMaybe "" blog) <> "\t" <> "\n" <>
    htmlUrl <> "\t" <> tshow createdAt <> "\n\n" <>
    (fromMaybe "" bio)
  where
    userName = Github.githubOwnerName user
    login = Github.githubOwnerLogin user
    company = Github.githubOwnerCompany user
    location = Github.githubOwnerLocation user
    blog = Github.githubOwnerBlog user
    htmlUrl = Github.githubOwnerHtmlUrl user
    createdAt = Github.githubOwnerCreatedAt user
    bio = Github.githubOwnerBio user

formatUser user@(Github.GithubUser {}) =
  (formatName userName login) <> "\t" <> (fromMaybe "" company) <> "\t" <>
    (fromMaybe "" location) <> "\n" <>
    (fromMaybe "" blog) <> "\t" <> "<" <> (fromMaybe "" email) <> ">" <> "\n" <>
    htmlUrl <> "\t" <> tshow createdAt <> "\n" <>
    "hireable: " <> formatHireable (fromMaybe False isHireable) <> "\n\n" <>
    (fromMaybe "" bio)
  where
    userName = Github.githubOwnerName user
    login = Github.githubOwnerLogin user
    company = Github.githubOwnerCompany user
    location = Github.githubOwnerLocation user
    blog = Github.githubOwnerBlog user
    email = Github.githubOwnerEmail user
    htmlUrl = Github.githubOwnerHtmlUrl user
    createdAt = Github.githubOwnerCreatedAt user
    isHireable = Github.githubOwnerHireable user
    bio = Github.githubOwnerBio user

formatName :: Maybe Text -> Github.Name Github.GithubOwner -> Text
formatName Nothing login = Github.untagName login
formatName (Just name) login = name <> "(" <> Github.untagName login <> ")"

formatHireable :: Bool -> Text
formatHireable True = "yes"
formatHireable False = "no"
