-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
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

-- | Create a new status
--
-- > createStatus (BasicAuth user password) "thoughtbot" "paperclip"
-- >   "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
-- >   (NewStatus StatusSuccess Nothing "Looks good!" Nothing)
createStatus :: Auth -> Name Owner -> Name Repo -> Name Commit -> NewStatus -> IO (Either Error Status)
createStatus auth owner repo sha ns =
    executeRequest auth $ createStatusR owner repo sha ns

-- | Create a new status
-- See <https://developer.github.com/v3/repos/statuses/#create-a-status>
createStatusR :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> Request 'RW Status
createStatusR owner repo sha =
    command Post parts . encode
    where
        parts = ["repos", toPathPart owner, toPathPart repo, "statuses", toPathPart sha]

-- | All statuses for a commit
--
-- > statusesFor (BasicAuth user password) "thoughtbot" "paperclip"
-- >   "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
statusesFor :: Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error (Vector Status))
statusesFor auth user repo sha =
    executeRequest auth $ statusesForR user repo sha FetchAll

-- | All statuses for a commit
-- See <https://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref>
statusesForR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request 'RW (Vector Status)
statusesForR user repo sha =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "statuses"] []

-- | The combined status for a specific commit
--
-- > statusFor (BasicAuth user password) "thoughtbot" "paperclip"
-- >   "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
statusFor :: Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error CombinedStatus)
statusFor auth user repo sha =
    executeRequest auth $ statusForR user repo sha

-- | The combined status for a specific commit
-- See <https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref>
statusForR :: Name Owner -> Name Repo -> Name Commit -> Request 'RW CombinedStatus
statusForR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "status"] []
