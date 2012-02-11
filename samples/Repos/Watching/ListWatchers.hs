module ListWatchers where

import qualified Github.Repos.Watching as Github
import Data.List (intercalate)
import Data.Default (def)

main = do
  possibleWatchers <- Github.watchersFor def "doubledrones" "git-annex"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n" . map formatWatcher)
                    possibleWatchers

formatWatcher :: Github.GithubOwner -> String
formatWatcher user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
