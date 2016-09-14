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

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

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
    executeRequestMaybe auth $ labelsOnRepoR user repo FetchAll

-- | List all labels for this repository.
-- See <https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository>
labelsOnRepoR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector IssueLabel)
labelsOnRepoR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "labels"] []

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
    query ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] []

-- | Create a label
--
-- > createLabel (User (user password)) "thoughtbot" "paperclip" "bug" "f29513"
createLabel :: Auth -> Name Owner -> Name Repo -> Name IssueLabel -> String -> IO (Either Error IssueLabel)
createLabel auth user repo lbl color =
    executeRequest auth $ createLabelR user repo lbl color

-- | Create a label.
-- See <https://developer.github.com/v3/issues/labels/#create-a-label>
createLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> String -> Request 'RW IssueLabel
createLabelR user repo lbl color =
    command Post paths $ encode body
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
             -> Request 'RW IssueLabel
updateLabelR user repo oldLbl newLbl color =
    command Patch paths (encode body)
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
deleteLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> Request 'RW ()
deleteLabelR user repo lbl =
    command Delete ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] mempty

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
    executeRequestMaybe auth $ labelsOnIssueR user repo iid FetchAll

-- | List labels on an issue.
-- See <https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue>
labelsOnIssueR :: Name Owner -> Name Repo -> Id Issue -> FetchCount -> Request k (Vector IssueLabel)
labelsOnIssueR user repo iid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] []

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
                  -> Request 'RW (Vector IssueLabel)
addLabelsToIssueR user repo iid lbls =
    command Post paths (encode $ toList lbls)
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
removeLabelFromIssueR :: Name Owner -> Name Repo -> Id Issue -> Name IssueLabel -> Request 'RW ()
removeLabelFromIssueR user repo iid lbl =
    command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels", toPathPart lbl] mempty

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
                          -> Request 'RW (Vector IssueLabel)
replaceAllLabelsForIssueR user repo iid lbls =
    command Put paths (encode $ toList lbls)
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
removeAllLabelsFromIssueR :: Name Owner -> Name Repo -> Id Issue -> Request 'RW ()
removeAllLabelsFromIssueR user repo iid =
    command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] mempty

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
    executeRequestMaybe auth $ labelsOnMilestoneR user repo mid FetchAll

-- | Query labels for every issue in a milestone.
-- See <https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone>
labelsOnMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> FetchCount -> Request k (Vector IssueLabel)
labelsOnMilestoneR user repo mid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid, "labels"] []
