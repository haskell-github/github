module Github.Issues.Milestones (
 milestones
,milestone
,module Github.Data
) where

import Github.Data
import Github.Private

milestones :: String -> String -> IO (Either Error [Milestone])
milestones user repoName = githubGet ["repos", user, repoName, "milestones"]

milestone :: String -> String -> Int -> IO (Either Error Milestone)
milestone user repoName milestoneNumber =
  githubGet ["repos", user, repoName, "milestones", show milestoneNumber]
