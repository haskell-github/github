module ListForks where

import qualified Github.Repos.Forks as Github
import Data.List

main = do
  possibleForks <- Github.forksFor "thoughtbot" "paperclip"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n\n" . map formatFork)
                    possibleForks

formatFork fork =
  (Github.githubOwnerLogin $ Github.repoOwner fork) ++ "\t" ++
  (formatPushedAt $ Github.repoPushedAt fork) ++ "\n" ++
  (formatCloneUrl $ Github.repoCloneUrl fork)

formatPushedAt Nothing         = ""
formatPushedAt (Just pushedAt) = show $ Github.fromDate pushedAt

formatCloneUrl Nothing         = ""
formatCloneUrl (Just cloneUrl) = cloneUrl
