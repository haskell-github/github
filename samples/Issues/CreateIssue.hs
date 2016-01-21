{-# LANGUAGE OverloadedStrings #-}
module CreateIssue where

import qualified Github.Auth   as Github
import qualified Github.Issues as Github
main = do
  let auth = Github.BasicAuth "user" "password"
      newiss = (Github.newIssue "A new issue") {
        Github.newIssueBody = Just "Issue description text goes here"
        }
  possibleIssue <- Github.createIssue auth "thoughtbot" "paperclip" newiss
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatIssue
                    possibleIssue

formatIssue issue =
  (Github.githubOwnerLogin $ Github.issueUser issue) ++
    " opened this issue " ++
    (show $ Github.fromDate $ Github.issueCreatedAt issue) ++ "\n" ++
    (Github.issueState issue) ++ " with " ++
    (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
    (Github.issueTitle issue)
