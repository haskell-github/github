module MergePullRequest where

import qualified Github.PullRequests as Github
import Github.Auth

main :: IO ()
main = do
  mergeResult  <- Github.mergePullRequest (OAuth "authtoken") "thoughtbot" "paperclip" 575 (Just "Merge message")
  case mergeResult of
       (Left err) -> putStrLn $ "Error: " ++ (show err)
       (Right stat) -> putStrLn $ (show stat)
