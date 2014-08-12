module MergePullRequest where

import qualified Github.PullRequests as Github
import Github.Auth

main :: IO ()
main = do
  mergeResult  <- Github.updatePullRequest (GithubOAuth "authtoken") "repoOwner" "repoName" 22 (EditPullRequest { editPullRequestTitle = Just "Brand new title", editPullRequestBody = Nothing, editPullRequestState = Just EditPullRequestStateClosed })
  case mergeResult of
       (Left err) -> putStrLn $ "Error: " ++ (show err)
       (Right dpr) -> putStrLn . show $ dpr
