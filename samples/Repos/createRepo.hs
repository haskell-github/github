{-# LANGUAGE OverloadedStrings #-}
module Main where

import GitHub.Endpoints.Repos

-- the code below results in this repo: https://github.com/shapr/some_repo

-- https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line
-- give the token "public_repo" access
-- should be a 40 character string
myauth = OAuth "80....................................2e" -- magically converts to ByteString

mynewrepo = NewRepo { newRepoName = mkName ([] :: [Repo]) "some_repo"
                    , newRepoDescription = Just "some description"
                    , newRepoHomepage = Nothing
                    , newRepoPrivate = Just False
                    , newRepoHasIssues = Just False
                    , newRepoHasWiki = Just False
                    , newRepoAutoInit = Just True -- please create a README.md for me!
                    }

main :: IO ()
main = do
  result <- createRepo' myauth mynewrepo
  print result
