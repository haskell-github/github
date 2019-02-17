-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo starring API as described on
-- <https://developer.github.com/v3/activity/starring/>.
module GitHub.Endpoints.Activity.Starring (
    stargazersFor,
    stargazersForR,
    reposStarredBy,
    reposStarredByR,
    myStarred,
    myStarredR,
    myStarredAcceptStar,
    myStarredAcceptStarR,
    starRepo,
    starRepoR,
    unstarRepo,
    unstarRepoR,
    module GitHub.Data,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | The list of users that have starred the specified Github repo.
--
-- > userInfoFor' Nothing "mike-burns"
stargazersFor :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector SimpleUser))
stargazersFor auth user repo =
    executeRequestMaybe auth $ stargazersForR user repo FetchAll

-- | List Stargazers.
-- See <https://developer.github.com/v3/activity/starring/#list-stargazers>
stargazersForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector SimpleUser)
stargazersForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "stargazers"] []

-- | All the public repos starred by the specified user.
--
-- > reposStarredBy Nothing "croaky"
reposStarredBy :: Maybe Auth -> Name Owner -> IO (Either Error (Vector Repo))
reposStarredBy auth user =
    executeRequestMaybe auth $ reposStarredByR user FetchAll

-- | List repositories being starred.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
reposStarredByR :: Name Owner -> FetchCount -> Request k (Vector Repo)
reposStarredByR user =
    pagedQuery ["users", toPathPart user, "starred"] []

-- | All the repos starred by the authenticated user.
myStarred :: Auth -> IO (Either Error (Vector Repo))
myStarred auth =
    executeRequest auth $ myStarredR FetchAll

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
myStarredR :: FetchCount -> Request 'RA (Vector Repo)
myStarredR = pagedQuery ["user", "starred"] []


-- | All the repos starred by the authenticated user.
myStarredAcceptStar :: Auth -> IO (Either Error (Vector RepoStarred))
myStarredAcceptStar auth =
    executeRequest auth $ myStarredAcceptStarR FetchAll

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1>
myStarredAcceptStarR :: FetchCount -> GenRequest 'MtStar 'RA (Vector RepoStarred)
myStarredAcceptStarR = PagedQuery ["user", "starred"] []

-- | Star a repo by the authenticated user.
starRepo :: Auth -> Name Owner -> Name Repo -> IO (Either Error ())
starRepo auth user repo = executeRequest auth $ starRepoR user repo

-- | Star a repo by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#star-a-repository>
starRepoR :: Name Owner -> Name Repo -> GenRequest 'MtUnit 'RW ()
starRepoR user repo = Command Put paths mempty
  where
    paths = ["user", "starred", toPathPart user, toPathPart repo]

-- | Unstar a repo by the authenticated user.
unstarRepo :: Auth -> Name Owner -> Name Repo -> IO (Either Error ())
unstarRepo auth user repo = executeRequest auth $ unstarRepoR user repo

-- | Unstar a repo by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#unstar-a-repository>
unstarRepoR :: Name Owner -> Name Repo -> Request 'RW ()
unstarRepoR user repo = command Delete paths mempty
  where
    paths = ["user", "starred", toPathPart user, toPathPart repo]
