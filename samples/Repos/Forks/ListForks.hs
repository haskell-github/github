module ListForks where

import qualified Github.Repos.Forks as Github
import Data.List
import Data.Default (def)

main = do
  possibleForks <- Github.forksFor def "thoughtbot" "paperclip"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n\n" . map formatFork)
                    possibleForks

formatFork fork =
  (Github.githubOwnerLogin $ Github.repoOwner fork) ++ "\t" ++
  (show $ Github.fromGithubDate $ Github.repoPushedAt fork) ++ "\n" ++
  (Github.repoCloneUrl fork)
