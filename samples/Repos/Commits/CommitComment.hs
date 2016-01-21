module CommitComment where

import qualified Github.Repos.Commits as Github
import Data.Maybe (maybe)

main = do
  possibleComment <- Github.commitCommentFor "thoughtbot" "paperclip" "669575"
  case possibleComment of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right comment) -> putStrLn $ formatComment comment

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    (maybe "" ("\nURL: "++) $ Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.Owner -> String
formatAuthor user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
