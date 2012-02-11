module ShowIssueLabels where

import qualified Github.Issues.Labels as Github
import Data.List (intercalate)
import Data.Default (def)

main = do
  possibleLabels <- Github.labelsOnIssue def "thoughtbot" "paperclip" 585
  case possibleLabels of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right labels) -> do
         putStrLn $ intercalate "\n" $ map formatLabel labels

formatLabel label =
  (Github.labelName label) ++ ", colored " ++ (Github.labelColor label)
