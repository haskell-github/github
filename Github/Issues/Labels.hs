{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The API for dealing with labels on Github issues as described on
-- <http://developer.github.com/v3/issues/labels/>.
module Github.Issues.Labels (
    labelsOnRepo,
    labelsOnRepo',
    labelsOnRepoR,
    label,
    label',
    labelR,
    createLabel,
    createLabelR,
    updateLabel,
    updateLabelR,
    deleteLabel,
    deleteLabelR,
    labelsOnIssue,
    labelsOnIssue',
    labelsOnIssueR,
    addLabelsToIssue,
    addLabelsToIssueR,
    removeLabelFromIssue,
    removeLabelFromIssueR,
    replaceAllLabelsForIssue,
    replaceAllLabelsForIssueR,
    removeAllLabelsFromIssue,
    removeAllLabelsFromIssueR,
    labelsOnMilestone,
    labelsOnMilestone',
    labelsOnMilestoneR,
    module Github.Data,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat (encode, object, (.=))
import Data.Foldable     (toList)
import Github.Auth
import Github.Data
import Github.Request
import Data.Vector (Vector)

-- | All the labels available to use on any issue in the repo.
--
-- > labelsOnRepo "thoughtbot" "paperclip"
labelsOnRepo :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector IssueLabel))
labelsOnRepo = labelsOnRepo' Nothing

-- | All the labels available to use on any issue in the repo using authentication.
--
-- > labelsOnRepo' (Just (GithubUser (user password))) "thoughtbot" "paperclip"
labelsOnRepo' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector IssueLabel))
labelsOnRepo' auth user repo =
    executeRequestMaybe auth $ labelsOnRepoR user repo Nothing

-- | List all labels for this repository.
-- See <https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository>
labelsOnRepoR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector IssueLabel)
labelsOnRepoR user repo =
    GithubPagedGet ["repos", untagName user, untagName repo, "labels"] []

-- | A label by name.
--
-- > label "thoughtbot" "paperclip" "bug"
label :: Name GithubOwner -> Name Repo -> Name IssueLabel -> IO (Either Error IssueLabel)
label = label' Nothing

-- | A label by name using authentication.
--
-- > label' (Just (GithubUser (user password))) "thoughtbot" "paperclip" "bug"
label' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name IssueLabel -> IO (Either Error IssueLabel)
label' auth user repo lbl =
    executeRequestMaybe auth $ labelR user repo lbl

-- | Get a single label.
-- See <https://developer.github.com/v3/issues/labels/#get-a-single-label>
labelR :: Name GithubOwner -> Name Repo -> Name IssueLabel -> GithubRequest k IssueLabel
labelR user repo lbl =
    GithubGet ["repos", untagName user, untagName repo, "labels", untagName lbl] []

-- | Create a label
--
-- > createLabel (GithubUser (user password)) "thoughtbot" "paperclip" "bug" "f29513"
createLabel :: GithubAuth -> Name GithubOwner -> Name Repo -> Name IssueLabel -> String -> IO (Either Error IssueLabel)
createLabel auth user repo lbl color =
    executeRequest auth $ createLabelR user repo lbl color

-- | Create a label.
-- See <https://developer.github.com/v3/issues/labels/#create-a-label>
createLabelR :: Name GithubOwner -> Name Repo -> Name IssueLabel -> String -> GithubRequest 'True IssueLabel
createLabelR user repo lbl color =
    GithubPost Post paths $ encode body
  where
    paths = ["repos", untagName user, untagName repo, "labels"]
    body = object ["name" .= untagName lbl, "color" .= color]

-- | Update a label
--
-- > updateLabel (GithubUser (user password)) "thoughtbot" "paperclip" "bug" "new-bug" "ff1111"
updateLabel :: GithubAuth
            -> Name GithubOwner
            -> Name Repo
            -> Name IssueLabel   -- ^ old label name
            -> Name IssueLabel   -- ^ new label name
            -> String            -- ^ new color
            -> IO (Either Error IssueLabel)
updateLabel auth user repo oldLbl newLbl color =
    executeRequest auth $ updateLabelR user repo oldLbl newLbl color

-- | Update a label.
-- See <https://developer.github.com/v3/issues/labels/#update-a-label>
updateLabelR :: Name GithubOwner
             -> Name Repo
             -> Name IssueLabel   -- ^ old label name
             -> Name IssueLabel   -- ^ new label name
             -> String            -- ^ new color
             -> GithubRequest 'True IssueLabel
updateLabelR user repo oldLbl newLbl color =
    GithubPost Patch paths (encode body)
  where
    paths = ["repos", untagName user, untagName repo, "labels", untagName oldLbl]
    body = object ["name" .= untagName newLbl, "color" .= color]

-- | Delete a label
--
-- > deleteLabel (GithubUser (user password)) "thoughtbot" "paperclip" "bug"
deleteLabel :: GithubAuth -> Name GithubOwner -> Name Repo -> Name IssueLabel -> IO (Either Error ())
deleteLabel auth user repo lbl =
    executeRequest auth $ deleteLabelR user repo lbl

-- | Delete a label.
-- See <https://developer.github.com/v3/issues/labels/#delete-a-label>
deleteLabelR :: Name GithubOwner -> Name Repo -> Name IssueLabel -> GithubRequest 'True ()
deleteLabelR user repo lbl =
    GithubDelete ["repos", untagName user, untagName repo, "labels", untagName lbl]

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue "thoughtbot" "paperclip" 585
labelsOnIssue :: Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueLabel))
labelsOnIssue = labelsOnIssue' Nothing

-- | The labels on an issue in a repo using authentication.
--
-- > labelsOnIssue' (Just (GithubUser (user password))) "thoughtbot" "paperclip" (Id 585)
labelsOnIssue' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueLabel))
labelsOnIssue' auth user repo iid =
    executeRequestMaybe auth $ labelsOnIssueR user repo iid Nothing

-- | List labels on an issue.
-- See <https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue>
labelsOnIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> Maybe Count -> GithubRequest k (Vector IssueLabel)
labelsOnIssueR user repo iid =
    GithubPagedGet ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "labels"] []

-- | Add labels to an issue.
--
-- > addLabelsToIssue (GithubUser (user password)) "thoughtbot" "paperclip" (Id 585) ["Label1" "Label2"]
addLabelsToIssue :: Foldable f
                 => GithubAuth
                 -> Name GithubOwner
                 -> Name Repo
                 -> Id Issue
                 -> f (Name IssueLabel)
                 -> IO (Either Error (Vector IssueLabel))
addLabelsToIssue auth user repo iid lbls =
    executeRequest auth $ addLabelsToIssueR user repo iid lbls

-- | Add lables to an issue.
-- See <https://developer.github.com/v3/issues/labels/#add-labels-to-an-issue>
addLabelsToIssueR :: Foldable f
                  => Name GithubOwner
                  -> Name Repo
                  -> Id Issue
                  -> f (Name IssueLabel)
                  -> GithubRequest 'True (Vector IssueLabel)
addLabelsToIssueR user repo iid lbls =
    GithubPost Post paths (encode $ toList lbls)
  where
    paths = ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "labels"]

-- | Remove a label from an issue.
--
-- > removeLabelFromIssue (GithubUser (user password)) "thoughtbot" "paperclip" (Id 585) "bug"
removeLabelFromIssue :: GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> Name IssueLabel -> IO (Either Error ())
removeLabelFromIssue auth user repo iid lbl =
    executeRequest auth $ removeLabelFromIssueR user repo iid lbl

-- | Remove a label from an issue.
-- See <https://developer.github.com/v3/issues/labels/#remove-a-label-from-an-issue>
removeLabelFromIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> Name IssueLabel -> GithubRequest 'True ()
removeLabelFromIssueR user repo iid lbl =
    GithubDelete ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "labels", untagName lbl]

-- | Replace all labels on an issue. Sending an empty list will remove all labels from the issue.
--
-- > replaceAllLabelsForIssue (GithubUser (user password)) "thoughtbot" "paperclip" (Id 585) ["Label1" "Label2"]
replaceAllLabelsForIssue :: Foldable f
                         => GithubAuth
                         -> Name GithubOwner
                         -> Name Repo
                         -> Id Issue
                         -> f (Name IssueLabel)
                         -> IO (Either Error (Vector IssueLabel))
replaceAllLabelsForIssue auth user repo iid lbls =
    executeRequest auth $ replaceAllLabelsForIssueR user repo iid lbls

-- | Replace all labels on an issue.
-- See <https://developer.github.com/v3/issues/labels/#replace-all-labels-for-an-issue>
--
-- Sending an empty list will remove all labels from the issue.
replaceAllLabelsForIssueR :: Foldable f
                          => Name GithubOwner
                          -> Name Repo
                          -> Id Issue
                          -> f (Name IssueLabel)
                          -> GithubRequest 'True (Vector IssueLabel)
replaceAllLabelsForIssueR user repo iid lbls =
    GithubPost Put paths (encode $ toList lbls)
  where
    paths = ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "labels"]

-- | Remove all labels from an issue.
--
-- > removeAllLabelsFromIssue (GithubUser (user password)) "thoughtbot" "paperclip" (Id 585)
removeAllLabelsFromIssue :: GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error ())
removeAllLabelsFromIssue auth user repo iid =
    executeRequest auth $ removeAllLabelsFromIssueR user repo iid

-- | Remove all labels from an issue.
-- See <https://developer.github.com/v3/issues/labels/#remove-all-labels-from-an-issue>
removeAllLabelsFromIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> GithubRequest 'True ()
removeAllLabelsFromIssueR user repo iid =
    GithubDelete ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "labels"]

-- | All the labels on a repo's milestone given the milestone ID.
--
-- > labelsOnMilestone "thoughtbot" "paperclip" (Id 2)
labelsOnMilestone :: Name GithubOwner -> Name Repo -> Id Milestone -> IO (Either Error (Vector IssueLabel))
labelsOnMilestone = labelsOnMilestone' Nothing

-- | All the labels on a repo's milestone given the milestone ID using authentication.
--
-- > labelsOnMilestone' (Just (GithubUser (user password))) "thoughtbot" "paperclip" (Id 2)
labelsOnMilestone' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Milestone -> IO (Either Error (Vector IssueLabel))
labelsOnMilestone' auth user repo mid =
    executeRequestMaybe auth $ labelsOnMilestoneR user repo mid Nothing

-- | Get labels for every issue in a milestone.
-- See <https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone>
labelsOnMilestoneR :: Name GithubOwner -> Name Repo -> Id Milestone -> Maybe Count -> GithubRequest k (Vector IssueLabel)
labelsOnMilestoneR user repo mid =
    GithubPagedGet ["repos", untagName user, untagName repo, "milestones", show $ untagId mid, "labels"] []
