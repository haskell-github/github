module ShowRepo where

import qualified Github.Repos as Github
import Data.List
import Data.Maybe

main = do
  possibleRepo <- Github.repository "mike-burns" "trylambda"
  case possibleRepo of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right repo) -> putStrLn $ formatRepo repo

formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (fromMaybe "" $ Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (fromMaybe "" $ Github.repoCloneUrl repo) ++ "\t" ++
    (fromMaybe "" $ formatDate `fmap` Github.repoUpdatedAt repo) ++ "\n" ++
    formatLanguage (Github.repoLanguage repo) ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate = show . Github.fromDate

formatLanguage (Just language) = "language: " ++ language ++ "\t"
formatLanguage Nothing = ""
