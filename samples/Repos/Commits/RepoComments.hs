module RepoComments where

import qualified Github.Repos.Commits as Github
import Data.List
import Data.Maybe (maybe)

main = do
  possibleComments <- Github.commentsFor "thoughtbot" "paperclip"
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
