module ListWatched where

import qualified Github.Repos.Watching as Github
import Data.List (intercalate)

main = do
  possibleRepos <- Github.reposWatchedBy "mike-burns"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n\n" . map formatRepo)
                    possibleRepos

formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (Github.repoCloneUrl repo) ++ "\t" ++
    (formatDate $ Github.repoUpdatedAt repo) ++ "\n" ++
    "language: " ++ (Github.repoLanguage repo) ++ "\t" ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate = show . Github.fromGithubDate
