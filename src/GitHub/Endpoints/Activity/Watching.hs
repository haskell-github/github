-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo watching API as described on
-- <https://developer.github.com/v3/activity/watching/>.
module GitHub.Endpoints.Activity.Watching (
    watchersFor,
    watchersFor',
    watchersForR,
    reposWatchedBy,
    reposWatchedBy',
    reposWatchedByR,
    module GitHub.Data,
) where

import GitHub.Auth
import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | The list of users that are watching the specified Github repo.
--
-- > watchersFor "thoughtbot" "paperclip"
watchersFor :: Name Owner -> Name Repo -> IO (Either Error (Vector SimpleUser))
watchersFor = watchersFor' Nothing

-- | The list of users that are watching the specified Github repo.
-- With authentication
--
-- > watchersFor' (Just (User (user, password))) "thoughtbot" "paperclip"
watchersFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector SimpleUser))
watchersFor' auth user repo =
    executeRequestMaybe auth $ watchersForR user repo FetchAll

-- | List watchers.
-- See <https://developer.github.com/v3/activity/watching/#list-watchers>
watchersForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector SimpleUser)
watchersForR user repo limit =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "watchers"] [] limit

-- | All the public repos watched by the specified user.
--
-- > reposWatchedBy "croaky"
reposWatchedBy :: Name Owner -> IO (Either Error (Vector Repo))
reposWatchedBy = reposWatchedBy' Nothing

-- | All the public repos watched by the specified user.
-- With authentication
--
-- > reposWatchedBy' (Just (User (user, password))) "croaky"
reposWatchedBy' :: Maybe Auth -> Name Owner -> IO (Either Error (Vector Repo))
reposWatchedBy' auth user =
    executeRequestMaybe auth $ reposWatchedByR user FetchAll

-- | List repositories being watched.
-- See <https://developer.github.com/v3/activity/watching/#list-repositories-being-watched>
reposWatchedByR :: Name Owner -> FetchCount -> Request k (Vector Repo)
reposWatchedByR user =
    pagedQuery ["users", toPathPart user, "subscriptions"] []
