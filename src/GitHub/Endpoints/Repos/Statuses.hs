--
-- The repo statuses API as described on
-- <https://developer.github.com/v3/repos/statuses/>.
module GitHub.Endpoints.Repos.Statuses (
    createStatus,
    createStatusR,
    statusesFor,
    statusesForR,
    statusFor,
    statusForR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

createStatus :: Auth -> Name Owner -> Name Repo -> Name Commit -> NewStatus -> IO (Either Error Status)
createStatus auth owner repo sha ns =
    executeRequest auth $ createStatusR owner repo sha ns

createStatusR :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> Request 'RW Status
createStatusR owner repo sha =
    command Post parts . encode
    where
        parts = ["repos", toPathPart owner, toPathPart repo, "statuses", toPathPart sha]

statusesFor :: Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error (Vector Status))
statusesFor auth user repo sha =
    executeRequest auth $ statusesForR user repo sha FetchAll

statusesForR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request 'RW (Vector Status)
statusesForR user repo sha =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "statuses"] []

statusFor :: Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error CombinedStatus)
statusFor auth user repo sha =
    executeRequest auth $ statusForR user repo sha

statusForR :: Name Owner -> Name Repo -> Name Commit -> Request 'RW CombinedStatus
statusForR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "status"] []
