module GitLsTree where

import qualified Github.GitData.Trees as Github
import Data.List (intercalate)

main = do
  possibleTree <- Github.tree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
  case possibleTree of
       (Left error)  -> putStrLn $ "Error: " ++ show error
       (Right tree) -> putStrLn $ formatTree tree

formatTree tree =
  intercalate "\n" $ map formatGitTree $ Github.treeGitTrees tree

formatGitTree gitTree =
  (Github.gitTreeMode gitTree) ++ " " ++
    (Github.gitTreeType gitTree) ++ " " ++
    (Github.gitTreeSha gitTree) ++ "\t" ++
    (Github.gitTreePath gitTree)
