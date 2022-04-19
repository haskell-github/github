{-# LANGUAGE OverloadedStrings #-}
module SearchIssues where

import qualified Github.Search as Github
import qualified Data.Text as T
import Control.Monad (forM_)

main :: IO ()
main = do
  let query = "q=build%20repo%3Aphadej%2Fgithub&per_page=100"
  result <- Github.github' Github.searchIssuesR query 1000
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do
      forM_ (Github.searchResultResults r) $ \r -> do
        putStrLn $ formatIssue r
        putStrLn ""
      putStrLn $ "Count: " ++ show (Github.searchResultTotalCount r)
        ++ " matches for the query: \"" ++ T.unpack query ++ "\""

formatIssue :: Github.Issue -> String
formatIssue issue =
  (show $ Github.issueUser issue) <>
    " opened this issue " <>
    (show $ Github.issueCreatedAt issue) <> "\n" <>
    (show $ Github.issueState issue) <> " with " <>
    (show $ Github.issueComments issue) <> " comments" <> "\n\n" <>
    (T.unpack $ Github.issueTitle issue)
