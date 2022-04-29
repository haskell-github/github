{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GitHub
import qualified Data.Text as T
import Control.Monad (forM_)

main :: IO ()
main = do
  let query = "build repo:haskell-github/github"
  result <- GitHub.github' GitHub.searchIssuesR query 1000
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do
      forM_ (GitHub.searchResultResults r) $ \r -> do
        putStrLn $ formatIssue r
        putStrLn ""
      putStrLn $ "Count: " ++ show (GitHub.searchResultTotalCount r)
        ++ " matches for the query: \"" ++ T.unpack query ++ "\""

formatIssue :: GitHub.Issue -> String
formatIssue issue =
  (show $ GitHub.issueUser issue) <>
    " opened this issue " <>
    (show $ GitHub.issueCreatedAt issue) <> "\n" <>
    (show $ GitHub.issueState issue) <> " with " <>
    (show $ GitHub.issueComments issue) <> " comments" <> "\n\n" <>
    (T.unpack $ GitHub.issueTitle issue)
