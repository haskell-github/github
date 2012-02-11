module ListBranches where

import qualified Github.Repos as Github
import Data.List
import Data.Default (def)

main = do
  possibleBranches <- Github.branchesFor def "thoughtbot" "paperclip"
  case possibleBranches of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right branches) ->
         putStrLn $ intercalate "\n" $ map Github.branchName branches
