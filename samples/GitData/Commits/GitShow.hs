module GitShow where

import qualified Github.GitData.Commits as Github
import Data.Maybe (fromJust)

main = do
  possibleCommit <- Github.commit "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
  case possibleCommit of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right commit) -> putStrLn $ formatCommit commit

formatCommit :: Github.GitCommit -> String
formatCommit commit =
  "commit " ++ (fromJust $ Github.gitCommitSha commit) ++
    "\nAuthor: " ++ (formatAuthor author) ++
    "\nDate:   " ++ (show $ Github.fromDate $ Github.gitUserDate author) ++
    "\n\n\t" ++ (Github.gitCommitMessage commit) ++ "\n"
  where author = Github.gitCommitAuthor commit

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  (Github.gitUserName author) ++ " <" ++ (Github.gitUserEmail author) ++ ">"

