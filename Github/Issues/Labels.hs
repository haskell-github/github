{-# LANGUAGE OverloadedStrings #-}
-- | The API for dealing with labels on Github issues, as described on
-- <http://developer.github.com/v3/issues/labels/>.
module Github.Issues.Labels (
 labelsOnRepo
,labelsOnRepo'
,label
,label'
,createLabel
,updateLabel
,deleteLabel
,labelsOnIssue
,labelsOnIssue'
,addLabelsToIssue
,removeLabelFromIssue
,replaceAllLabelsForIssue
,removeAllLabelsFromIssue
,labelsOnMilestone
,labelsOnMilestone'
,module Github.Data
) where

import Data.Aeson     (object, (.=))
import Github.Data
import Github.Private

-- | All the labels available to use on any issue in the repo.
--
-- > labelsOnRepo "thoughtbot" "paperclip"
labelsOnRepo :: String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo = labelsOnRepo' Nothing

-- | All the labels available to use on any issue in the repo, using authentication.
--
-- > labelsOnRepo' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
labelsOnRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo' auth user reqRepoName =
  githubGet' auth ["repos", user, reqRepoName, "labels"]

-- | A label, by name.
--
-- > label "thoughtbot" "paperclip" "bug"
label :: String -> String -> String -> IO (Either Error IssueLabel)
label = label' Nothing

-- | A label, by name, using authentication.
--
-- > label' (Just (GithubUser (user, password))) "thoughtbot" "paperclip" "bug"
label' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error IssueLabel)
label' auth user reqRepoName reqLabelName =
  githubGet' auth ["repos", user, reqRepoName, "labels", reqLabelName]

-- | Create a label
--
-- > createLabel (GithubUser (user, password)) "thoughtbot" "paperclip" "bug" "f29513"
createLabel :: GithubAuth -> String -> String -> String -> String -> IO (Either Error IssueLabel)
createLabel auth reqUserName reqRepoName reqLabelName reqLabelColor = githubPost auth paths body
  where
    paths = ["repos", reqUserName, reqRepoName, "labels"]
    body = object ["name" .= reqLabelName, "color" .= reqLabelColor]

-- | Update a label
--
-- > updateLabel (GithubUser (user, password)) "thoughtbot" "paperclip" "bug" "new-bug" "ff1111"
updateLabel :: GithubAuth -> String -> String -> String -> String -> String -> IO (Either Error IssueLabel)
updateLabel auth reqUserName reqRepoName oldLabelName newLabelName reqLabelColor = githubPatch auth paths body
  where
    paths = ["repos", reqUserName, reqRepoName, "labels", oldLabelName]
    body = object ["name" .= newLabelName, "color" .= reqLabelColor]

-- | Delete a label
--
-- > deleteLabel (GithubUser (user, password)) "thoughtbot" "paperclip" "bug"
deleteLabel :: GithubAuth -> String -> String -> String -> IO (Either Error ())
deleteLabel auth reqUserName reqRepoName reqLabelName = githubDelete auth paths
  where
    paths = ["repos", reqUserName, reqRepoName, "labels", reqLabelName]

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue "thoughtbot" "paperclip" 585
labelsOnIssue :: String -> String -> Int -> IO (Either Error [IssueLabel])
labelsOnIssue = labelsOnIssue' Nothing

-- | The labels on an issue in a repo, using authentication.
--
-- > labelsOnIssue' (Just (GithubUser (user, password))) "thoughtbot" "paperclip" 585
labelsOnIssue' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [IssueLabel])
labelsOnIssue' auth user reqRepoName reqIssueId =
  githubGet' auth ["repos", user, reqRepoName, "issues", show reqIssueId, "labels"]

-- | Add labels to an issue.
--
-- > addLabelsToIssue (GithubUser (user, password)) "thoughtbot" "paperclip" 585 ["Label1", "Label2"]
addLabelsToIssue :: GithubAuth -> String -> String -> Int -> [String] -> IO (Either Error [IssueLabel])
addLabelsToIssue auth user reqRepoName reqIssueId = githubPost auth paths
  where
    paths =["repos", user, reqRepoName, "issues", show reqIssueId, "labels"]

-- | Remove a label from an issue.
--
-- > removeLabelFromIssue (GithubUser (user, password)) "thoughtbot" "paperclip" 585 "bug"
removeLabelFromIssue :: GithubAuth -> String -> String -> Int -> String -> IO (Either Error ())
removeLabelFromIssue auth user reqRepoName reqIssueId reqLabelName = githubDelete auth paths
  where
    paths =["repos", user, reqRepoName, "issues", show reqIssueId, "labels", reqLabelName]

-- | Replace all labels on an issue. Sending an empty list will remove all labels from the issue.
--
-- > replaceAllLabelsForIssue (GithubUser (user, password)) "thoughtbot" "paperclip" 585 ["Label1", "Label2"]
replaceAllLabelsForIssue :: GithubAuth -> String -> String -> Int -> [String] -> IO (Either Error [IssueLabel])
replaceAllLabelsForIssue auth user reqRepoName reqIssueId = githubPut auth paths
  where
    paths =["repos", user, reqRepoName, "issues", show reqIssueId, "labels"]

-- | Remove all labels from an issue.
--
-- > removeAllLabelsFromIssue (GithubUser (user, password)) "thoughtbot" "paperclip" 585
removeAllLabelsFromIssue :: GithubAuth -> String -> String -> Int -> IO (Either Error ())
removeAllLabelsFromIssue auth user reqRepoName reqIssueId = githubDelete auth paths
  where
    paths =["repos", user, reqRepoName, "issues", show reqIssueId, "labels"]

-- | All the labels on a repo's milestone, given the milestone ID.
--
-- > labelsOnMilestone "thoughtbot" "paperclip" 2
labelsOnMilestone :: String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnMilestone = labelsOnMilestone' Nothing

-- | All the labels on a repo's milestone, given the milestone ID, using authentication.
--
-- > labelsOnMilestone' (Just (GithubUser (user, password))) "thoughtbot" "paperclip" 2
labelsOnMilestone' :: Maybe GithubAuth -> String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnMilestone' auth user reqRepoName milestoneId =
  githubGet' auth ["repos", user, reqRepoName, "milestones", show milestoneId, "labels"]
