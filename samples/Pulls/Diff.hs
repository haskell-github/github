module Diff where

import qualified Github.PullRequests as Github
import Data.List

main = do
  possiblePullRequestFiles <- Github.pullRequestFiles "thoughtbot" "paperclip" 575
  case possiblePullRequestFiles of
       (Left error)    -> putStrLn $ "Error: " ++ (show error)
       (Right files) -> putStrLn $ intercalate "\n\n" $ map formatFile files

formatFile file =
  Github.fileFilename file ++ "\n" ++ Github.filePatch file
