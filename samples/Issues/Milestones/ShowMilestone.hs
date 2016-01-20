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
    formatDueOn (Github.milestoneDueOn milestone) ++ " and has the " ++
    (Github.milestoneState milestone) ++ " status"

formatDueOn Nothing = ""
formatDueOn (Just milestoneDate) = ", is due on " ++ dueOn milestoneDate

loginName = Github.githubOwnerLogin . Github.milestoneCreator
createdAt = show . Github.fromDate . Github.milestoneCreatedAt
dueOn = show . Github.fromDate
