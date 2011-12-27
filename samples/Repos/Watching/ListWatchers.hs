module ListWatchers where

import qualified Github.Repos.Watching as Github
import Data.List (intercalate)

main = do
  possibleWatchers <- Github.watchersFor "thoughtbot" "paperclip"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n" . map formatWatcher)
                    possibleWatchers

formatWatcher :: Github.GithubUser -> String
formatWatcher user =
  (Github.githubUserLogin user) ++ " (" ++ (Github.githubUserUrl user) ++ ")"
