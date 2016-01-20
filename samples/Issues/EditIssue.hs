{-# LANGUAGE OverloadedStrings #-}
module EditIssue where

import qualified Github.Auth as Github
import qualified Github.Issues as Github

main = do
  let auth = Github.BasicAuth "user" "password"
      issueid = 3
      edit = Github.editOfIssue { Github.editIssueState = Just "closed" }
  possibleIssue <- Github.editIssue auth "thoughtbot" "paperclip" issueid edit
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
