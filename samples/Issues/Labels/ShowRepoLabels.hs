module ShowRepoLabels where

import qualified Github.Issues.Labels as Github
import Data.List (intercalate)

main = do
  possibleLabels <- Github.labelsOnRepo "thoughtbot" "paperclip"
  case possibleLabels of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right labels) -> do
         putStrLn $ intercalate "\n" $ map formatLabel labels

formatLabel label =
  (Github.labelName label) ++ ", colored " ++ (Github.labelColor label)
