module GitShow where

import qualified Github.Repos.Commits as Github
import Data.List

main = do
  possibleCommit <- Github.commit "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
  case possibleCommit of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right commit) -> putStrLn $ formatCommit commit

formatCommit :: Github.Commit -> String
formatCommit commit =
  "commit " ++ (Github.commitSha commit) ++
    "\nAuthor: " ++ (formatAuthor author) ++
    "\nDate:   " ++ (show $ Github.fromDate $ Github.gitUserDate author) ++
    "\n\n\t" ++ (Github.gitCommitMessage gitCommit) ++ "\n" ++
    patches
  where author = Github.gitCommitAuthor gitCommit
        gitCommit = Github.commitGitCommit commit
        patches = 
          intercalate "\n" $ map Github.filePatch $ Github.commitFiles commit

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  (Github.gitUserName author) ++ " <" ++ (Github.gitUserEmail author) ++ ">"

