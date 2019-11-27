-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The milestones API as described on
-- <http://developer.github.com/v3/issues/milestones/>.
module GitHub.Endpoints.Issues.Milestones (
    milestonesR,
    milestoneR,
    createMilestoneR,
    updateMilestoneR,
    deleteMilestoneR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List milestones for a repository.
-- See <https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository>
milestonesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Milestone)
milestonesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "milestones"] []

-- | Query a single milestone.
-- See <https://developer.github.com/v3/issues/milestones/#get-a-single-milestone>
milestoneR :: Name Owner -> Name Repo -> Id Milestone -> Request k Milestone
milestoneR user repo mid =
    query ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid] []

-- | Create a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#create-a-milestone>
createMilestoneR :: Name Owner -> Name Repo -> NewMilestone -> Request 'RW Milestone
createMilestoneR user repo =
    command Post ["repos", toPathPart user, toPathPart repo, "milestones"] . encode

-- | Update a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#update-a-milestone>
updateMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> UpdateMilestone -> Request 'RW Milestone
updateMilestoneR user repo mid =
    command Patch ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid ] . encode

-- | Delete a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#delete-a-milestone>
deleteMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> GenRequest 'MtUnit 'RW ()
deleteMilestoneR user repo mid =
    Command Delete
        ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid] mempty
