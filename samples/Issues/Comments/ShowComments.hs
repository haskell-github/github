module ShowComments where

import qualified Github.Issues.Comments as Github
import Data.List (intercalate)

main = do
  possibleComments <- Github.comments "thoughtbot" "paperclip" 635
  case possibleComments of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right issues) ->
         putStrLn $ intercalate "\n\n" $ map formatComment issues

formatComment comment =
  (Github.githubOwnerLogin $ Github.issueCommentUser comment) ++
    " commented " ++
    (show $ Github.fromDate $ Github.issueCommentUpdatedAt comment) ++
    "\n" ++ (Github.issueCommentBody comment)
