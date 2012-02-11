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
-- > milestones def "thoughtbot" "paperclip"
milestones :: GithubConfig -> String -> String -> IO (Either Error [Milestone])
milestones c user repoName =
  githubGet c ["repos", user, repoName, "milestones"]

-- | Details on a specific milestone, given it's milestone number.
--
-- > milestone def "thoughtbot" "paperclip" 2
milestone :: GithubConfig -> String -> String -> Int -> IO (Either Error Milestone)
milestone c user repoName milestoneNumber =
  githubGet c ["repos", user, repoName, "milestones", show milestoneNumber]
