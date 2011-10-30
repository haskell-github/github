module Sample where

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
    "\nDate:   " ++ (show $ Github.authorDate author) ++
    "\n\n\t" ++ (Github.commitMessage commit)
  where author = Github.commitAuthor commit

formatAuthor :: Github.Author -> String
formatAuthor author =
  (Github.authorName author) ++ " <" ++ (Github.authorEmail author) ++ ">"
