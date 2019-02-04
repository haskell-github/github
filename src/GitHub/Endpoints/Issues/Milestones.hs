-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The milestones API as described on
-- <http://developer.github.com/v3/issues/milestones/>.
module GitHub.Endpoints.Issues.Milestones (
    milestones,
    milestones',
    milestonesR,
    milestone,
    milestoneR,
    createMilestone,
    createMilestoneR,
    updateMilestone,
    updateMilestoneR,
    deleteMilestone,
    deleteMilestoneR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All milestones in the repo.
--
-- > milestones "thoughtbot" "paperclip"
milestones :: Name Owner -> Name Repo -> IO (Either Error (Vector Milestone))
milestones = milestones' Nothing

-- | All milestones in the repo, using authentication.
--
-- > milestones' (User (user, passwordG) "thoughtbot" "paperclip"
milestones' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Milestone))
milestones' auth user repo =
    executeRequestMaybe auth $ milestonesR user repo FetchAll

-- | List milestones for a repository.
-- See <https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository>
milestonesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Milestone)
milestonesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "milestones"] []

-- | Details on a specific milestone, given it's milestone number.
--
-- > milestone "thoughtbot" "paperclip" (Id 2)
milestone :: Name Owner -> Name Repo -> Id Milestone -> IO (Either Error Milestone)
milestone user repo mid =
    executeRequest' $ milestoneR user repo mid

-- | Query a single milestone.
-- See <https://developer.github.com/v3/issues/milestones/#get-a-single-milestone>
milestoneR :: Name Owner -> Name Repo -> Id Milestone -> Request k Milestone
milestoneR user repo mid =
    query ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid] []

createMilestone :: Auth -> Name Owner -> Name Repo -> NewMilestone -> IO (Either Error Milestone)
createMilestone auth user repo mlstn = executeRequest auth $ createMilestoneR user repo mlstn

-- | Create a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#create-a-milestone>
createMilestoneR :: Name Owner -> Name Repo -> NewMilestone -> Request 'RW Milestone
createMilestoneR user repo =
    command Post ["repos", toPathPart user, toPathPart repo, "milestones"] . encode

updateMilestone :: Auth -> Name Owner -> Name Repo -> Id Milestone -> UpdateMilestone -> IO (Either Error Milestone)
updateMilestone auth user repo mid mlstn = executeRequest auth $ updateMilestoneR user repo mid mlstn

-- | Update a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#update-a-milestone>
updateMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> UpdateMilestone -> Request 'RW Milestone
updateMilestoneR user repo mid =
    command Patch ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid ] . encode

deleteMilestone :: Auth -> Name Owner -> Name Repo -> Id Milestone -> IO (Either Error ())
deleteMilestone auth user repo mid = executeRequest auth $ deleteMilestoneR user repo mid

-- | Delete a milestone.
-- See <https://developer.github.com/v3/issues/milestones/#delete-a-milestone>
deleteMilestoneR :: Name Owner -> Name Repo -> Id Milestone -> Request 'RW ()
deleteMilestoneR user repo mid =
    command Delete
        ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid] mempty
