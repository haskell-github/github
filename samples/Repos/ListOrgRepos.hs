module ListOrgRepos where

import qualified Github.Repos as Github
import Data.List
import Data.Default (def)

main = do
  possibleRepos <- Github.organizationRepos def "thoughtbot"
  case possibleRepos of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo repos

formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (Github.repoCloneUrl repo) ++ "\t" ++
    (formatDate $ Github.repoUpdatedAt repo) ++ "\n" ++
    formatLanguage (Github.repoLanguage repo) ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate = show . Github.fromGithubDate

formatLanguage (Just language) = "language: " ++ language ++ "\t"
formatLanguage Nothing = ""
