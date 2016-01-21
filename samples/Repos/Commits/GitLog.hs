module GitLog where

import qualified Github.Repos.Commits as Github
import Data.List

main = do
  possibleCommits <- Github.commitsFor "thoughtbot" "paperclip"
  case possibleCommits of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right commits) -> putStrLn $ intercalate "\n\n" $ map formatCommit commits

formatCommit :: Github.Commit -> String
formatCommit commit =
  "commit " ++ (Github.commitSha commit) ++
    "\nAuthor: " ++ (formatAuthor author) ++
    "\nDate:   " ++ (show $ Github.fromDate $ Github.gitUserDate author) ++
    "\n\n\t" ++ (Github.gitCommitMessage gitCommit)
  where author = Github.gitCommitAuthor gitCommit
        gitCommit = Github.commitGitCommit commit

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  (Github.gitUserName author) ++ " <" ++ (Github.gitUserEmail author) ++ ">"
