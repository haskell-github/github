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
