-- | The milestones API as described on
-- <http://developer.github.com/v3/issues/milestones/>.
module Github.Issues.Milestones (
 milestones
,milestones'
,milestone
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All milestones in the repo.
--
-- > milestones "thoughtbot" "paperclip"
milestones :: String -> String -> IO (Either Error [Milestone])
milestones = milestones' Nothing

-- | All milestones in the repo, using authentication.
--
-- > milestones' (GithubUser (user, password)) "thoughtbot" "paperclip"
milestones' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Milestone])
milestones' auth user reqRepoName = githubGet' auth ["repos", user, reqRepoName, "milestones"]

-- | Details on a specific milestone, given it's milestone number.
--
-- > milestone "thoughtbot" "paperclip" 2
milestone :: String -> String -> Int -> IO (Either Error Milestone)
milestone user reqRepoName reqMilestoneNumber =
  githubGet ["repos", user, reqRepoName, "milestones", show reqMilestoneNumber]
