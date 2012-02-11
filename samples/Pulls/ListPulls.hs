module ListPulls where

import qualified Github.PullRequests as Github
import Data.List
import Data.Default (def)

main = do
  possiblePullRequests <- Github.pullRequestsFor def "thoughtbot" "paperclip"
  case possiblePullRequests of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right pullRequests) ->
         putStrLn $ intercalate "\n\n" $ map formatPullRequest pullRequests

formatPullRequest pullRequest =
  (Github.pullRequestTitle pullRequest) ++ "\n" ++
    (take 80 $ Github.pullRequestBody pullRequest) ++ "\n" ++
    (Github.githubOwnerLogin $ Github.pullRequestUser pullRequest) ++
    " submitted to thoughtbot/paperclip " ++
    (formatGithubDate $ Github.pullRequestCreatedAt pullRequest) ++
    " updated " ++
    (formatGithubDate $ Github.pullRequestUpdatedAt pullRequest) ++ "\n" ++
    (Github.pullRequestHtmlUrl pullRequest)

formatGithubDate = show . Github.fromGithubDate
