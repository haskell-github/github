-- | The API for dealing with labels on Github issues, as described on
-- <http://developer.github.com/v3/issues/labels/>.
module Github.Issues.Labels (
 label
,labelsOnRepo
,labelsOnIssue
,labelsOnMilestone
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the labels available to use on any issue in the repo.
--
-- > labelsOnRepo "thoughtbot" "paperclip"
labelsOnRepo :: String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo user repoName = githubGet ["repos", user, repoName, "labels"]

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue "thoughtbot" "paperclip" 585
labelsOnIssue :: String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnIssue user repoName issueId =
  githubGet ["repos", user, repoName, "issues", show issueId, "labels"]

-- | All the labels on a repo's milestone, given the milestone ID.
--
-- > labelsOnMilestone "thoughtbot" "paperclip" 2
labelsOnMilestone :: String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnMilestone user repoName milestoneId =
  githubGet ["repos", user, repoName, "milestones", show milestoneId, "labels"]

-- | A label, by name.
--
-- > Github.label "thoughtbot" "paperclip" "bug"
label :: String -> String -> String -> IO (Either Error IssueLabel)
label user repoName labelName =
  githubGet ["repos", user, repoName, "labels", labelName]
