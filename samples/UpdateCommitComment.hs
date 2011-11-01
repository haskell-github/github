module UpdateCommitComment where

import qualified Github.Repos.Commits as Github

main = do
  let commentUpdater = Github.updateCommentWith "mike-burns" "github" "746779d28dbbeece2593ba37a30a1b457edf3f6e"
  didItWork <- commentUpdater "Actually, that was a poor idea"
  putStrLn $ either (\error   -> "Error: "  ++ (show error))
                    (\comment -> "Posted: " ++ (show comment))
                    didItWork
