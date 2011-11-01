module CommitComment where

import qualified Github.Repos.Commits as Github

main = do
  possibleComment <- Github.commitCommentFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
  case possibleComment of
    (Left error)    -> putStrLn $ "Error: " ++ (show error)
    (Right comment) -> putStrLn $ formatComment comment

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    "\nURL: " ++ (Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.GithubUser -> String
formatAuthor user =
  (Github.githubUserLogin user) ++ " (" ++ (Github.githubUserUrl user) ++ ")"
