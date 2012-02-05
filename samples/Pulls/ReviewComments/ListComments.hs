module ListComments where

import qualified Github.PullRequests.ReviewComments as Github
import Data.List

main = do
  possiblePullRequestComments <- Github.pullRequestReviewComments "thoughtbot" "factory_girl" 256
  case possiblePullRequestComments of
       (Left error)     -> putStrLn $ "Error: " ++ (show error)
       (Right comments) -> putStrLn $ intercalate "\n\n" $ map formatComment comments

formatComment :: Github.Comment -> String
formatComment comment =
  "Author: " ++ (formatAuthor $ Github.commentUser comment) ++
    "\nUpdated: " ++ (show $ Github.commentUpdatedAt comment) ++
    (maybe "" ("\nURL: "++) $ Github.commentHtmlUrl comment) ++
    "\n\n" ++ (Github.commentBody comment)

formatAuthor :: Github.GithubOwner -> String
formatAuthor user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
