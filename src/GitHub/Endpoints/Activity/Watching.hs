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

import Data.Vector    (Vector)
import GitHub.Auth
import GitHub.Data
import GitHub.Request

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
    executeRequestMaybe auth $ watchersForR user repo Nothing

-- | List watchers.
-- See <https://developer.github.com/v3/activity/watching/#list-watchers>
watchersForR :: Name Owner -> Name Repo -> Maybe Count -> Request k (Vector SimpleUser)
watchersForR user repo limit =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "watchers"] [] limit

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
    executeRequestMaybe auth $ reposWatchedByR user Nothing

-- | List repositories being watched.
-- See <https://developer.github.com/v3/activity/watching/#list-repositories-being-watched>
reposWatchedByR :: Name Owner -> Maybe Count -> Request k (Vector Repo)
reposWatchedByR user =
    PagedQuery ["users", toPathPart user, "subscriptions"] []
