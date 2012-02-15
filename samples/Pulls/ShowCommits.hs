module ShowCommits where

import qualified Github.PullRequests as Github
import Data.List
import Data.Maybe

main = do
  possiblePullRequestCommits <- Github.pullRequestCommits "thoughtbot" "paperclip" 575
  case possiblePullRequestCommits of
       (Left error)    -> putStrLn $ "Error: " ++ (show error)
       (Right commits) -> putStrLn $ intercalate "\n" $ map formatCommit commits

formatCommit commit =
  (formatAuthor $ Github.gitCommitAuthor gitCommit) ++ "\n" ++
    (maybe "unknown SHA" id $ Github.gitCommitSha gitCommit) ++ "\n" ++
    (Github.gitCommitMessage gitCommit)
  where gitCommit = Github.commitGitCommit commit

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  (Github.gitUserName author) ++ " <" ++ (Github.gitUserEmail author) ++ ">"
