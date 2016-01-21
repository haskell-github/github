{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The API for dealing with labels on Github issues as described on
-- <http://developer.github.com/v3/issues/labels/>.
module GitHub.Endpoints.Issues.Labels (
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
    module GitHub.Data,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat (encode, object, (.=))
import Data.Foldable     (toList)
import Data.Vector       (Vector)

import GitHub.Data
import GitHub.Request

-- | All the labels available to use on any issue in the repo.
--
-- > labelsOnRepo "thoughtbot" "paperclip"
labelsOnRepo :: Name Owner -> Name Repo -> IO (Either Error (Vector IssueLabel))
labelsOnRepo = labelsOnRepo' Nothing

-- | All the labels available to use on any issue in the repo using authentication.
--
-- > labelsOnRepo' (Just (User (user password))) "thoughtbot" "paperclip"
labelsOnRepo' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector IssueLabel))
labelsOnRepo' auth user repo =
    executeRequestMaybe auth $ labelsOnRepoR user repo Nothing

-- | List all labels for this repository.
-- See <https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository>
labelsOnRepoR :: Name Owner -> Name Repo -> Maybe Count -> Request k (Vector IssueLabel)
labelsOnRepoR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "labels"] []

-- | A label by name.
--
-- > label "thoughtbot" "paperclip" "bug"
label :: Name Owner -> Name Repo -> Name IssueLabel -> IO (Either Error IssueLabel)
label = label' Nothing

-- | A label by name using authentication.
--
-- > label' (Just (User (user password))) "thoughtbot" "paperclip" "bug"
label' :: Maybe Auth -> Name Owner -> Name Repo -> Name IssueLabel -> IO (Either Error IssueLabel)
label' auth user repo lbl =
    executeRequestMaybe auth $ labelR user repo lbl

-- | Query a single label.
-- See <https://developer.github.com/v3/issues/labels/#get-a-single-label>
labelR :: Name Owner -> Name Repo -> Name IssueLabel -> Request k IssueLabel
labelR user repo lbl =
    Query ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] []

-- | Create a label
--
-- > createLabel (User (user password)) "thoughtbot" "paperclip" "bug" "f29513"
createLabel :: Auth -> Name Owner -> Name Repo -> Name IssueLabel -> String -> IO (Either Error IssueLabel)
createLabel auth user repo lbl color =
    executeRequest auth $ createLabelR user repo lbl color

-- | Create a label.
-- See <https://developer.github.com/v3/issues/labels/#create-a-label>
createLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> String -> Request 'True IssueLabel
createLabelR user repo lbl color =
    Command Post paths $ encode body
  where
    paths = ["repos", toPathPart user, toPathPart repo, "labels"]
    body = object ["name" .= untagName lbl, "color" .= color]

-- | Update a label
--
-- > updateLabel (User (user password)) "thoughtbot" "paperclip" "bug" "new-bug" "ff1111"
updateLabel :: Auth
            -> Name Owner
            -> Name Repo
            -> Name IssueLabel   -- ^ old label name
            -> Name IssueLabel   -- ^ new label name
            -> String            -- ^ new color
            -> IO (Either Error IssueLabel)
updateLabel auth user repo oldLbl newLbl color =
    executeRequest auth $ updateLabelR user repo oldLbl newLbl color

-- | Update a label.
-- See <https://developer.github.com/v3/issues/labels/#update-a-label>
updateLabelR :: Name Owner
             -> Name Repo
             -> Name IssueLabel   -- ^ old label name
             -> Name IssueLabel   -- ^ new label name
             -> String            -- ^ new color
             -> Request 'True IssueLabel
updateLabelR user repo oldLbl newLbl color =
    Command Patch paths (encode body)
  where
    paths = ["repos", toPathPart user, toPathPart repo, "labels", toPathPart oldLbl]
    body = object ["name" .= untagName newLbl, "color" .= color]

-- | Delete a label
--
-- > deleteLabel (User (user password)) "thoughtbot" "paperclip" "bug"
deleteLabel :: Auth -> Name Owner -> Name Repo -> Name IssueLabel -> IO (Either Error ())
deleteLabel auth user repo lbl =
    executeRequest auth $ deleteLabelR user repo lbl

-- | Delete a label.
-- See <https://developer.github.com/v3/issues/labels/#delete-a-label>
deleteLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> Request 'True ()
deleteLabelR user repo lbl =
    Command Delete ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] mempty

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue "thoughtbot" "paperclip" 585
labelsOnIssue :: Name Owner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueLabel))
labelsOnIssue = labelsOnIssue' Nothing

-- | The labels on an issue in a repo using authentication.
--
-- > labelsOnIssue' (Just (User (user password))) "thoughtbot" "paperclip" (Id 585)
labelsOnIssue' :: Maybe Auth -> Name Owner -> Name Repo -> Id Issue -> IO (Either Error (Vector IssueLabel))
labelsOnIssue' auth user repo iid =
    executeRequestMaybe auth $ labelsOnIssueR user repo iid Nothing

-- | List labels on an issue.
-- See <https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue>
labelsOnIssueR :: Name Owner -> Name Repo -> Id Issue -> Maybe Count -> Request k (Vector IssueLabel)
labelsOnIssueR user repo iid =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] []

-- | Add labels to an issue.
--
-- > addLabelsToIssue (User (user password)) "thoughtbot" "paperclip" (Id 585) ["Label1" "Label2"]
addLabelsToIssue :: Foldable f
                 => Auth
                 -> Name Owner
                 -> Name Repo
                 -> Id Issue
                 -> f (Name IssueLabel)
                 -> IO (Either Error (Vector IssueLabel))
addLabelsToIssue auth user repo iid lbls =
    executeRequest auth $ addLabelsToIssueR user repo iid lbls

-- | Add lables to an issue.
-- See <https://developer.github.com/v3/issues/labels/#add-labels-to-an-issue>
addLabelsToIssueR :: Foldable f
                  => Name Owner
                  -> Name Repo
                  -> Id Issue
                  -> f (Name IssueLabel)
                  -> Request 'True (Vector IssueLabel)
addLabelsToIssueR user repo iid lbls =
    Command Post paths (encode $ toList lbls)
  where
    paths = ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"]

-- | Remove a label from an issue.
--
-- > removeLabelFromIssue (User (user password)) "thoughtbot" "paperclip" (Id 585) "bug"
removeLabelFromIssue :: Auth -> Name Owner -> Name Repo -> Id Issue -> Name IssueLabel -> IO (Either Error ())
removeLabelFromIssue auth user repo iid lbl =
    executeRequest auth $ removeLabelFromIssueR user repo iid lbl

-- | Remove a label from an issue.
-- See <https://developer.github.com/v3/issues/labels/#remove-a-label-from-an-issue>
removeLabelFromIssueR :: Name Owner -> Name Repo -> Id Issue -> Name IssueLabel -> Request 'True ()
removeLabelFromIssueR user repo iid lbl =
    Command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels", toPathPart lbl] mempty

-- | Replace all labels on an issue. Sending an empty list will remove all labels from the issue.
--
-- > replaceAllLabelsForIssue (User (user password)) "thoughtbot" "paperclip" (Id 585) ["Label1" "Label2"]
replaceAllLabelsForIssue :: Foldable f
                         => Auth
                         -> Name Owner
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
                          => Name Owner
                          -> Name Repo
                          -> Id Issue
                          -> f (Name IssueLabel)
                          -> Request 'True (Vector IssueLabel)
replaceAllLabelsForIssueR user repo iid lbls =
    Command Put paths (encode $ toList lbls)
  where
    paths = ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"]

-- | Remove all labels from an issue.
--
-- > removeAllLabelsFromIssue (User (user password)) "thoughtbot" "paperclip" (Id 585)
removeAllLabelsFromIssue :: Auth -> Name Owner -> Name Repo -> Id Issue -> IO (Either Error ())
removeAllLabelsFromIssue auth user repo iid =
    executeRequest auth $ removeAllLabelsFromIssueR user repo iid

-- | Remove all labels from an issue.
-- See <https://developer.github.com/v3/issues/labels/#remove-all-labels-from-an-issue>
removeAllLabelsFromIssueR :: Name Owner -> Name Repo -> Id Issue -> Request 'True ()
removeAllLabelsFromIssueR user repo iid =
    Command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] mempty

-- | All the labels on a repo's milestone given the milestone ID.
--
-- > labelsOnMilestone "thoughtbot" "paperclip" (Id 2)
labelsOnMilestone :: Name Owner -> Name Repo -> Id Milestone -> IO (Either Error (Vector IssueLabel))
labelsOnMilestone = labelsOnMilestone' Nothing

-- | All the labels on a repo's milestone given the milestone ID using authentication.
--
-- > labelsOnMilestone' (Just (User (user password))) "thoughtbot" "paperclip" (Id 2)
labelsOnMilestone' :: Maybe Auth -> Name Owner -> Name Repo -> Id Milestone -> IO (Either Error (Vector IssueLabel))
labelsOnMilestone' auth user repo mid =
    executeRequestMaybe auth $ labelsOnMilestoneR user repo mid Nothing

-- | Query labels for every issue in a milestone.
-- See <https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone>
labelsOnMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> Maybe Count -> Request k (Vector IssueLabel)
labelsOnMilestoneR user repo mid =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid, "labels"] []
