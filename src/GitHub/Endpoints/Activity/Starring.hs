-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo starring API as described on
-- <https://developer.github.com/v3/activity/starring/>.
module GitHub.Endpoints.Activity.Starring (
    stargazersForR,
    reposStarredByR,
    myStarredR,
    myStarredAcceptStarR,
    starRepoR,
    unstarRepoR,
    module GitHub.Data,
    ) where

import GitHub.Auth
import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List Stargazers.
-- See <https://developer.github.com/v3/activity/starring/#list-stargazers>
stargazersForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector SimpleUser)
stargazersForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "stargazers"] []

-- | List repositories being starred.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
reposStarredByR :: Name Owner -> FetchCount -> Request k (Vector Repo)
reposStarredByR user =
    pagedQuery ["users", toPathPart user, "starred"] []

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#list-repositories-being-starred>
myStarredR :: FetchCount -> Request 'RA (Vector Repo)
myStarredR = pagedQuery ["user", "starred"] []

-- | All the repos starred by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1>
myStarredAcceptStarR :: FetchCount -> GenRequest 'MtStar 'RA (Vector RepoStarred)
myStarredAcceptStarR = PagedQuery ["user", "starred"] []

-- | Star a repo by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#star-a-repository>
starRepoR :: Name Owner -> Name Repo -> GenRequest 'MtUnit 'RW ()
starRepoR user repo = Command Put paths mempty
  where
    paths = ["user", "starred", toPathPart user, toPathPart repo]

-- | Unstar a repo by the authenticated user.
-- See <https://developer.github.com/v3/activity/starring/#unstar-a-repository>
unstarRepoR :: Name Owner -> Name Repo -> GenRequest 'MtUnit 'RW ()
unstarRepoR user repo = Command Delete paths mempty
  where
    paths = ["user", "starred", toPathPart user, toPathPart repo]
