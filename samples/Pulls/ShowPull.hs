module ShowPull where

import qualified Github.PullRequests as Github
import Data.List

main = do
  possiblePullRequest <- Github.pullRequest "thoughtbot" "paperclip" 575
  case possiblePullRequest of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right pullRequest) -> putStrLn $ formatPullRequest pullRequest

formatPullRequest p =
  (Github.githubOwnerLogin $ Github.detailedPullRequestUser p) ++
    " opened this pull request " ++
    (formatDate $ Github.detailedPullRequestCreatedAt p) ++ "\n" ++
    (Github.detailedPullRequestTitle p) ++ "\n" ++
    (Github.detailedPullRequestBody p) ++ "\n" ++
    (Github.detailedPullRequestState p) ++ "\n" ++
    "+" ++ (show $ Github.detailedPullRequestAdditions p) ++ " additions\n" ++
    "-" ++ (show $ Github.detailedPullRequestDeletions p) ++ " deletions\n" ++
    (show $ Github.detailedPullRequestComments p) ++ " comments\n" ++
    (Github.detailedPullRequestHtmlUrl p)

formatDate = show . Github.fromDate
