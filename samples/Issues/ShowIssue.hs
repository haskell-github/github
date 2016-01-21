module ShowIssue where

import qualified Github.Issues as Github

main = do
  possibleIssue <- Github.issue "thoughtbot" "paperclip" 549
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
