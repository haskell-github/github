module ShowComments where

import qualified Github.PullRequests.ReviewComments as Github
import Data.List

main = do
  possiblePullRequestComment <- Github.pullRequestReviewComment "thoughtbot" "factory_girl" 301819
  case possiblePullRequestComment of
       (Left error)     -> putStrLn $ "Error: " ++ (show error)
       (Right comment) -> putStrLn $ formatComment comment

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    (maybe "" ("\nURL: "++) $ Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.GithubOwner -> String
formatAuthor user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"

