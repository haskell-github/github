--
-- The repo statuses API as described on
-- <https://developer.github.com/v3/repos/statuses/>.
module GitHub.Endpoints.Repos.Statuses (
    createStatus,
    createStatusR,
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
