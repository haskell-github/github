-- | The milestones API as described on
-- <http://developer.github.com/v3/issues/milestones/>.
module Github.Issues.Milestones (
 milestones
,milestone
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All milestones in the repo.
--
-- > milestones "thoughtbot" "paperclip"
milestones :: String -> String -> IO (Either Error [Milestone])
milestones user repoName = githubGet ["repos", user, repoName, "milestones"]

-- | Details on a specific milestone, given it's milestone number.
--
-- > milestone "thoughtbot" "paperclip" 2
milestone :: String -> String -> Int -> IO (Either Error Milestone)
milestone user repoName milestoneNumber =
  githubGet ["repos", user, repoName, "milestones", show milestoneNumber]
