module GitDiff where

import qualified Github.Repos.Commits as Github
import Data.List

main = do
  possibleDiff <- Github.diff "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9" "HEAD"
  either (\error -> putStrLn $ "Error: " ++ (show error))
         (putStrLn . showDiff)
         possibleDiff

showDiff diff =
 intercalate "\n\n" $ map Github.filePatch $ Github.diffFiles diff
