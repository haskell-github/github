-- |
-- The repo statuses API as described on
-- <https://developer.github.com/v3/repos/statuses/>.

module GitHub.Endpoints.Repos.Statuses (
    createStatusR,
    statusesForR,
    statusForR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Create a new status
-- See <https://developer.github.com/v3/repos/statuses/#create-a-status>
createStatusR :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> Request 'RW Status
createStatusR owner repo sha =
    command Post parts . encode
    where
        parts = ["repos", toPathPart owner, toPathPart repo, "statuses", toPathPart sha]

-- | All statuses for a commit
-- See <https://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref>
statusesForR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request 'RW (Vector Status)
statusesForR user repo sha =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "statuses"] []

-- | The combined status for a specific commit
-- See <https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref>
statusForR :: Name Owner -> Name Repo -> Name Commit -> Request 'RW CombinedStatus
statusForR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "status"] []
