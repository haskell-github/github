module DeleteComment where

import qualified Github.Repos.Commits as Github

main = do
  didItWork <- Github.deleteComment "mike-burns" "github" "746779d28dbbeece2593ba37a30a1b457edf3f6e"
  putStrLn $ either (\error -> "Error: "  ++ (show error))
                    (const "Comment deleted")
                    didItWork
