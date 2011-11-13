module ShowMilestone where

import qualified Github.Issues.Milestones as Github
import Data.List (intercalate)

main = do
  possibleMilestone <- Github.milestone "thoughtbot" "paperclip" 2
  case possibleMilestone of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right milestone) ->
         putStrLn $ formatMilestone milestone

formatMilestone milestone =
  (Github.milestoneTitle milestone) ++ ", as created by " ++
    (loginName milestone) ++ " on " ++ (createdAt milestone) ++
    ", is due on " ++ (dueOn milestone) ++ " and has the " ++
    (Github.milestoneState milestone) ++ " status"

loginName = Github.githubUserLogin . Github.milestoneCreator
createdAt = show . Github.fromGithubDate . Github.milestoneCreatedAt
dueOn = show . Github.fromGithubDate . Github.milestoneDueOn

