{-# LANGUAGE OverloadedStrings #-}

import qualified GitHub as Github
import Data.List (intercalate)
import Data.Foldable (toList)

main :: IO ()
main = do
  let filt = Github.stateClosed <> Github.optionsMentioned "mike-burns" <> Github.optionsAssignee "jyurek"
  possibleIssues <- Github.github' $ Github.issuesForRepoR "thoughtbot" "paperclip" filt Github.FetchAll
  case possibleIssues of
       Left err -> putStrLn $ "Error: " ++ show err
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
  , " with "
  , show $ Github.issueComments issue
  , " comments.\n\n"

  , show $ Github.issueTitle issue
  ]
