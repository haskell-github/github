-- | The repo watching API as described on
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

import Github.Auth
import Github.Data
import Github.Request

-- | The list of users that are watching the specified Github repo.
--
-- > watchersFor "thoughtbot" "paperclip"
watchersFor :: Name GithubOwner -> Name Repo -> IO (Either Error [GithubOwner])
watchersFor = watchersFor' Nothing

-- | The list of users that are watching the specified Github repo.
-- With authentication
--
-- > watchersFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
watchersFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error [GithubOwner])
watchersFor' auth user repo =
    executeRequestMaybe auth $ watchersForR user repo

-- | List watchers.
-- See <https://developer.github.com/v3/activity/watching/#list-watchers>
watchersForR :: Name GithubOwner -> Name Repo -> GithubRequest k [GithubOwner]
watchersForR user repo =
    GithubGet ["repos", untagName user, untagName repo, "watchers"] ""

-- | All the public repos watched by the specified user.
--
-- > reposWatchedBy "croaky"
reposWatchedBy :: Name GithubOwner -> IO (Either Error [Repo])
reposWatchedBy = reposWatchedBy' Nothing

-- | All the public repos watched by the specified user.
-- With authentication
--
-- > reposWatchedBy' (Just (GithubUser (user, password))) "croaky"
reposWatchedBy' :: Maybe GithubAuth -> Name GithubOwner -> IO (Either Error [Repo])
reposWatchedBy' auth user =
    executeRequestMaybe auth $ reposWatchedByR user

-- | List repositories being watched.
-- See <https://developer.github.com/v3/activity/watching/#list-repositories-being-watched>
reposWatchedByR :: Name GithubOwner -> GithubRequest k [Repo]
reposWatchedByR user =
    GithubGet ["users", untagName user, "subscriptions"] ""
