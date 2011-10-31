module RepoComments where

import qualified Github.Repos.Commits as Github
import Data.List

main = do
  possibleComments <- Github.commentsFor "thoughtbot" "paperclip"
  case possibleComments of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right comments) -> putStrLn $ intercalate "\n\n" $  map formatComment comments

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    "\nURL: " ++ (Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.GithubUser -> String
formatAuthor user =
  (Github.githubUserLogin user) ++ " (" ++ (Github.githubUserUrl user) ++ ")"
