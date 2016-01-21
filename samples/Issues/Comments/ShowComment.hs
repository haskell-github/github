module ShowComment where

import qualified Github.Issues.Comments as Github

main = do
  possibleComment <- Github.comment "thoughtbot" "paperclip" 1468184
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatComment
                    possibleComment

formatComment comment =
  (Github.githubOwnerLogin $ Github.issueCommentUser comment) ++
    " commented " ++
    (show $ Github.fromDate $ Github.issueCommentUpdatedAt comment) ++
    "\n" ++ (Github.issueCommentBody comment)
