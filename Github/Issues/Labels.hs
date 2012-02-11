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
-- > labelsOnRepo def "thoughtbot" "paperclip"
labelsOnRepo :: GithubConfig -> String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo c user repoName =
  githubGet c ["repos", user, repoName, "labels"]

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue def "thoughtbot" "paperclip" 585
labelsOnIssue :: GithubConfig -> String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnIssue c user repoName issueId =
  githubGet c ["repos", user, repoName, "issues", show issueId, "labels"]

-- | All the labels on a repo's milestone, given the milestone ID.
--
-- > labelsOnMilestone def "thoughtbot" "paperclip" 2
labelsOnMilestone :: GithubConfig -> String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnMilestone c user repoName milestoneId =
  githubGet c ["repos", user, repoName, "milestones", show milestoneId, "labels"]

-- | A label, by name.
--
-- > Github.label def "thoughtbot" "paperclip" "bug"
label :: GithubConfig -> String -> String -> String -> IO (Either Error IssueLabel)
label c user repoName labelName =
  githubGet c ["repos", user, repoName, "labels", labelName]
