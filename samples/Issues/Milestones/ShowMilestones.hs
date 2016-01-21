module ShowMilestones where

import qualified Github.Issues.Milestones as Github
import Data.List (intercalate)

main = do
  possibleMilestones <- Github.milestones "thoughtbot" "paperclip"
  case possibleMilestones of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right milestones) ->
         putStrLn $ intercalate "\n\n" $ map formatMilestone milestones

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
