module ShowRepoLabels where

import           Data.List            (intercalate)
import qualified Github.Issues.Labels as Github

main = do
  possibleLabels <- Github.labelsOnRepo "thoughtbot" "paperclip"
  case possibleLabels of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right labels) -> do
         putStrLn $ intercalate "\n" $ map formatLabel labels

formatLabel label =
  (Github.labelName label) ++ ", colored " ++ (Github.labelColor label)
