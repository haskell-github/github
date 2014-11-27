-- | The repo subscribing API as described on
-- <http://developer.github.com/v3/repos/watching/>.
module Github.Repos.Subscribing (
 subscribersFor
,subscribersFor'
,reposSubscribedToBy
,reposSubscribedToBy'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of users that are subscribed to the specified Github repo.
--
-- > subscribersFor "thoughtbot" "paperclip"
subscribersFor :: String -> String -> IO (Either Error [GithubOwner])
subscribersFor = subscribersFor' Nothing

-- | The list of users that are subscribed to the specified Github repo.
-- | With authentication
--
-- > subscribersFor' (Just (GithubUser (user, password))) "thoughtbot" "paperclip"
subscribersFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [GithubOwner])
subscribersFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "subscribers"]

-- | All the public repos subscribed to by the specified user.
--
-- > reposSubscribedToBy "croaky"
reposSubscribedToBy :: String -> IO (Either Error [Repo])
reposSubscribedToBy = reposSubscribedToBy' Nothing

-- | All the public repos subscribed to by the specified user.
-- | With authentication
--
-- > reposSubscribedToBy' (Just (GithubUser (user, password))) "croaky"
reposSubscribedToBy' :: Maybe GithubAuth -> String -> IO (Either Error [Repo])
reposSubscribedToBy' auth userName = githubGet' auth ["users", userName, "subscriptions"]
