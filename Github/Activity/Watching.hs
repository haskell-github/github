-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo watching API as described on
-- <https://developer.github.com/v3/activity/watching/>.
module Github.Activity.Watching (
    watchersFor,
    watchersFor',
    watchersForR,
    reposWatchedBy,
    reposWatchedBy',
    reposWatchedByR,
    module Github.Data,
) where

import Data.Vector    (Vector)
import Github.Auth
import Github.Data
import Github.Request

-- | The list of users that are watching the specified Github repo.
--
-- > watchersFor "thoughtbot" "paperclip"
watchersFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimpleUser))
watchersFor = watchersFor' Nothing

-- | The list of users that are watching the specified Github repo.
-- With authentication
--
-- > watchersFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
watchersFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimpleUser))
watchersFor' auth user repo =
    executeRequestMaybe auth $ watchersForR user repo Nothing

-- | List watchers.
-- See <https://developer.github.com/v3/activity/watching/#list-watchers>
watchersForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector SimpleUser)
watchersForR user repo limit =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "watchers"] [] limit

-- | All the public repos watched by the specified user.
--
-- > reposWatchedBy "croaky"
reposWatchedBy :: Name GithubOwner -> IO (Either Error (Vector Repo))
reposWatchedBy = reposWatchedBy' Nothing

-- | All the public repos watched by the specified user.
-- With authentication
--
-- > reposWatchedBy' (Just (GithubUser (user, password))) "croaky"
reposWatchedBy' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error (Vector Repo))
reposWatchedBy' auth user =
    executeRequestMaybe auth $ reposWatchedByR user Nothing

-- | List repositories being watched.
-- See <https://developer.github.com/v3/activity/watching/#list-repositories-being-watched>
reposWatchedByR :: Name GithubOwner -> Maybe Count -> GithubRequest k (Vector Repo)
reposWatchedByR user =
    GithubPagedGet ["users", toPathPart user, "subscriptions"] []
