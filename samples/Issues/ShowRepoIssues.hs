{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (toList)
import Data.List     (intercalate)
import Data.Vector   (Vector)

import qualified GitHub as Github

main :: IO ()
main = do
  let filt = Github.stateClosed <> Github.optionsMentioned "mike-burns" <> Github.optionsAssignee "jyurek"
  printIssues =<< do
    Github.github' $ Github.issuesForRepoR "thoughtbot" "paperclip" filt Github.FetchAll

  printIssues =<< do
    Github.github' $ Github.issuesForRepoR "haskell-github" "playground" Github.stateClosed Github.FetchAll

printIssues :: Either Github.Error (Vector Github.Issue) -> IO ()
printIssues = \case
  Left err ->
    putStrLn $ "Error: " ++ show err
  Right issues ->
    putStrLn $ intercalate "\n\n" $ map formatIssue $ toList issues

formatIssue :: Github.Issue -> String
formatIssue issue = concat

  [ show $ Github.simpleUserLogin $ Github.issueUser issue
  , " opened this issue "
  , show $ Github.issueCreatedAt issue
  , ".\n"

  , "It is currently "
  , show $ Github.issueState issue
  , maybe "" (\ r -> " with reason " ++ show r) $ Github.issueStateReason issue
  , " with "
  , show $ Github.issueComments issue
  , " comments.\n\n"

  , show $ Github.issueTitle issue
  ]
