module ListWatchers where

import qualified Github.Repos.Watching as Github
import Data.List (intercalate)

main = do
  possibleWatchers <- Github.watchersFor "doubledrones" "git-annex"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n" . map formatWatcher)
                    possibleWatchers

formatWatcher :: Github.Owner -> String
formatWatcher user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
