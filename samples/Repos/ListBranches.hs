module ListBranches where

import qualified Github.Repos as Github
import Data.List

main = do
  possibleBranches <- Github.branchesFor "thoughtbot" "paperclip"
  case possibleBranches of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right branches) ->
         putStrLn $ intercalate "\n" $ map Github.branchName branches
