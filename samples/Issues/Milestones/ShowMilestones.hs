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
    ", is due on " ++ (dueOn milestone) ++ " and has the " ++
    (Github.milestoneState milestone) ++ " status"

loginName = Github.githubUserLogin . Github.milestoneCreator
createdAt = show . Github.fromGithubDate . Github.milestoneCreatedAt
dueOn = show . Github.fromGithubDate . Github.milestoneDueOn
