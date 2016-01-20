module CommitComments where

import qualified Github.Repos.Commits as Github
import Data.List
import Data.Maybe (maybe)

main = do
  possibleComments <- Github.commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
  case possibleComments of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right comments) -> putStrLn $ intercalate "\n\n" $  map formatComment comments

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    (maybe "" ("\nURL: "++) $ Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.Owner -> String
formatAuthor user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
