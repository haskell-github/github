{-# LANGUAGE OverloadedStrings #-}
module SearchIssues where

import qualified Github.Search as Github
import Control.Monad (forM_)

main = do
  let query = "q=build%20repo%3Aphadej%2Fgithub&per_page=100"
  let auth = Nothing
  result <- Github.searchIssues' auth query
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do forM_ (Github.searchIssuesIssues r) (\i -> do
                    putStrLn $ formatIssue i
                    putStrLn ""
                    )
                  putStrLn $ "Count: " ++ show n ++ " build issues"
      where n = Github.searchIssuesTotalCount r

formatIssue issue =
  (Github.githubOwnerLogin $ Github.issueUser issue) ++
    " opened this issue " ++
    (show $ Github.fromDate $ Github.issueCreatedAt issue) ++ "\n" ++
    (Github.issueState issue) ++ " with " ++
    (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
    (Github.issueTitle issue)
