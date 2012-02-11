module GitLsRemote where

import qualified Github.GitData.References as Github
import Data.List (intercalate)
import Data.Default (def)

main = do
  possibleReferences <- Github.references def "mike-burns" "github"
  case possibleReferences of
       (Left error)       -> putStrLn $ "Error: " ++ show error
       (Right references) -> do
         putStrLn "From git@github.com:mike-burns/github.git"
         putStrLn $ intercalate "\n" $ map formatReference references

formatReference reference =
  (Github.gitObjectSha $ Github.gitReferenceObject reference) ++
    "\t" ++ (Github.gitReferenceRef reference)
