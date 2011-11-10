module GitDiff where

import qualified Github.Repos.Commits as Github
import Data.List

main = do
  possibleDiff <- Github.diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
  either (\error -> putStrLn $ "Error: " ++ (show error))
         (putStrLn . showDiff)
         possibleDiff

showDiff diff =
 intercalate "\n\n" $ map Github.filePatch $ Github.diffFiles diff
