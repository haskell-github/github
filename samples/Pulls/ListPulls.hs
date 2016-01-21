module ListPulls where

import qualified Github.PullRequests as Github
import Data.List

main = do
  possiblePullRequests <- Github.pullRequestsFor "thoughtbot" "paperclip"
  case possiblePullRequests of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right pullRequests) ->
         putStrLn $ intercalate "\n\n" $ map formatPullRequest pullRequests

formatPullRequest pullRequest =
  (Github.pullRequestTitle pullRequest) ++ "\n" ++
    (take 80 $ Github.pullRequestBody pullRequest) ++ "\n" ++
    (Github.githubOwnerLogin $ Github.pullRequestUser pullRequest) ++
    " submitted to thoughtbot/paperclip " ++
    (formatDate $ Github.pullRequestCreatedAt pullRequest) ++
    " updated " ++
    (formatDate $ Github.pullRequestUpdatedAt pullRequest) ++ "\n" ++
    (Github.pullRequestHtmlUrl pullRequest)

formatDate = show . Github.fromDate
