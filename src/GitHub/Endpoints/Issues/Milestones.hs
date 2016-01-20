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

import Data.Vector (Vector)

import GitHub.Data
import GitHub.Request

-- | All milestones in the repo.
--
-- > milestones "thoughtbot" "paperclip"
milestones :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Milestone))
milestones = milestones' Nothing

-- | All milestones in the repo, using authentication.
--
-- > milestones' (GithubUser (user, passwordG) "thoughtbot" "paperclip"
milestones' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Milestone))
milestones' auth user repo =
    executeRequestMaybe auth $ milestonesR user repo Nothing

-- | List milestones for a repository.
-- See <https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository>
milestonesR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Milestone)
milestonesR user repo = GithubPagedGet ["repos", toPathPart user, toPathPart repo, "milestones"] []

-- | Details on a specific milestone, given it's milestone number.
--
-- > milestone "thoughtbot" "paperclip" (Id 2)
milestone :: Name GithubOwner -> Name Repo -> Id Milestone -> IO (Either Error Milestone)
milestone user repo mid =
    executeRequest' $ milestoneR user repo mid

-- | Get a single milestone.
-- See <https://developer.github.com/v3/issues/milestones/#get-a-single-milestone>
milestoneR :: Name GithubOwner -> Name Repo -> Id Milestone -> GithubRequest k Milestone
milestoneR user repo mid =
    GithubGet ["repos", toPathPart user, toPathPart repo, "milestones", toPathPart mid] []
