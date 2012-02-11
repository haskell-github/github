module GitLsRemoteWithRef where

import qualified Github.GitData.References as Github
import Data.Default (def)

main = do
  possibleReference <- Github.reference def "mike-burns" "github" "heads/master"
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatReference
                    possibleReference

formatReference reference =
  (Github.gitObjectSha $ Github.gitReferenceObject reference) ++
    "\t" ++ (Github.gitReferenceRef reference)
