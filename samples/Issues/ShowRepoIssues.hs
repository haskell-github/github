module ShowRepoIssue where

import qualified Github.Issues as Github
import Data.List (intercalate)

main = do
  let limitations = [Github.OnlyClosed, Github.Mentions "mike-burns", Github.AssignedTo "jyurek"]
  possibleIssues <- Github.issuesForRepo "thoughtbot" "paperclip" limitations
  case possibleIssues of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right issues) ->
         putStrLn $ intercalate "\n\n" $ map formatIssue issues

formatIssue issue =
  (Github.githubOwnerLogin $ Github.issueUser issue) ++
    " opened this issue " ++
    (show $ Github.fromDate $ Github.issueCreatedAt issue) ++ "\n" ++
    (Github.issueState issue) ++ " with " ++
    (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
    (Github.issueTitle issue)

