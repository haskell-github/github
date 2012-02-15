module GitLsRemoteTags where

import qualified Github.GitData.References as Github
import Data.List (intercalate)

main = do
  possibleReferences <- Github.namespacedReferences "thoughtbot" "paperclip" "tags"
  case possibleReferences of
       (Left error)       -> putStrLn $ "Error: " ++ show error
       (Right references) -> do
         putStrLn "From git@github.com:thoughtbot/paperclip.git"
         putStrLn $ intercalate "\n" $ map formatReference references

formatReference reference =
  (Github.gitObjectSha $ Github.gitReferenceObject reference) ++
    "\t" ++ (Github.gitReferenceRef reference)

