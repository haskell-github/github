module GitHub.Endpoints.Repos.Status (
      NewStatus(..)
    , createStatus
    , createStatusR
    , listStatuses
    , listStatusesR
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

createStatus :: Auth -> Name Owner -> Name Repo -> Name Commit -> NewStatus -> IO (Either Error Status)
createStatus auth user repo sha newStatus = executeRequest auth $ createStatusR user repo sha newStatus

createStatusR :: Name Owner -> Name Repo -> Name Commit -> NewStatus -> Request 'RW Status
createStatusR user repo sha = command Post paths . encode
  where
    paths = [ "repos", toPathPart user, toPathPart repo, "statuses", toPathPart sha ]

listStatuses :: Auth -> Name Owner -> Name Repo -> Name Commit -> FetchCount -> IO (Either Error (Vector Status))
listStatuses auth user repo sha n = executeRequest auth $ listStatusesR user repo sha n

listStatusesR :: Name Owner -> Name Repo -> Name Commit -> FetchCount -> Request 'RO (Vector Status)
listStatusesR user repo sha = pagedQuery paths []
  where
    paths = [ "repos", toPathPart user, toPathPart repo, "commits", toPathPart sha, "statuses" ]
