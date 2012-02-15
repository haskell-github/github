module ShowLabel where

import qualified Github.Issues.Labels as Github

main = do
  possibleLabel <- Github.label "thoughtbot" "paperclip" "bug"
  case possibleLabel of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right label) -> putStrLn $ formatLabel label

formatLabel label =
  (Github.labelName label) ++ ", colored " ++ (Github.labelColor label)

