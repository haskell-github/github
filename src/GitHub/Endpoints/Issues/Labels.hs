-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The API for dealing with labels on Github issues as described on
-- <http://developer.github.com/v3/issues/labels/>.
module GitHub.Endpoints.Issues.Labels (
    labelsOnRepoR,
    labelR,
    createLabelR,
    updateLabelR,
    deleteLabelR,
    labelsOnIssueR,
    addLabelsToIssueR,
    removeLabelFromIssueR,
    replaceAllLabelsForIssueR,
    removeAllLabelsFromIssueR,
    labelsOnMilestoneR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List all labels for this repository.
-- See <https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository>
labelsOnRepoR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector IssueLabel)
labelsOnRepoR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "labels"] []

-- | Query a single label.
-- See <https://developer.github.com/v3/issues/labels/#get-a-single-label>
labelR :: Name Owner -> Name Repo -> Name IssueLabel -> Request k IssueLabel
labelR user repo lbl =
    query ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] []

-- | Create a label.
-- See <https://developer.github.com/v3/issues/labels/#create-a-label>
createLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> String -> Request 'RW IssueLabel
createLabelR user repo lbl color =
    command Post paths $ encode body
  where
    paths = ["repos", toPathPart user, toPathPart repo, "labels"]
    body = object ["name" .= untagName lbl, "color" .= color]

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

-- | Delete a label.
-- See <https://developer.github.com/v3/issues/labels/#delete-a-label>
deleteLabelR :: Name Owner -> Name Repo -> Name IssueLabel -> GenRequest 'MtUnit 'RW ()
deleteLabelR user repo lbl =
    Command Delete ["repos", toPathPart user, toPathPart repo, "labels", toPathPart lbl] mempty

-- | List labels on an issue.
-- See <https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue>
labelsOnIssueR :: Name Owner -> Name Repo -> Id Issue -> FetchCount -> Request k (Vector IssueLabel)
labelsOnIssueR user repo iid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] []

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
-- See <https://developer.github.com/v3/issues/labels/#remove-a-label-from-an-issue>
removeLabelFromIssueR :: Name Owner -> Name Repo -> Id Issue -> Name IssueLabel -> GenRequest 'MtUnit 'RW ()
removeLabelFromIssueR user repo iid lbl =
    Command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels", toPathPart lbl] mempty

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
-- See <https://developer.github.com/v3/issues/labels/#remove-all-labels-from-an-issue>
removeAllLabelsFromIssueR :: Name Owner -> Name Repo -> Id Issue -> GenRequest 'MtUnit 'RW ()
removeAllLabelsFromIssueR user repo iid =
    Command Delete ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "labels"] mempty

-- | Query labels for every issue in a milestone.
-- See <https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone>
labelsOnMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> FetchCount -> Request k (Vector IssueLabel)
labelsOnMilestoneR user repo mid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid, "labels"] []
