-- | The traffic API, as described at <https://developer.github.com/v3/repos/traffic/>
module GitHub.Endpoints.Repos.Traffic (
    popularReferrers',
    popularReferrersR,
    popularPaths',
    popularPathsR,
    views',
    viewsR,
    clones',
    clonesR
    ) where

import Data.Vector (Vector)

import GitHub.Data
       (Auth, Clones, Error, Name, Owner, Period, PopularPath, Referrer, Repo,
       Views, prettyPeriod)
import GitHub.Data.Request (query, toPathPart)
import GitHub.Request      (Request, executeRequest)

-- | The top 10 referrers for the past 14 days.
--
-- > popularReferrers' (BasicAuth "github-username" "github-password") "qfpl" "tasty-hedgehog"
popularReferrers' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Referrer))
popularReferrers' auth user =
    executeRequest auth . popularReferrersR user

-- | The top 10 referrers for the past 14 days.
-- See <https://developer.github.com/v3/repos/traffic/#list-referrers>
popularReferrersR :: Name Owner -> Name Repo -> Request k (Vector Referrer)
popularReferrersR user repo =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "popular", "referrers"] []

-- | The 10 most popular paths based on visits over the last 14 days.
--
-- > popularPaths' (OAuth "supersecrettoken") "qfpl" "tasty-hedgehog"
popularPaths' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector PopularPath))
popularPaths' auth user =
    executeRequest auth . popularPathsR user

-- | The 10 most popular paths based on visits over the last 14 days.
-- See <https://developer.github.com/v3/repos/traffic/#list-paths>
popularPathsR :: Name Owner -> Name Repo -> Request k (Vector PopularPath)
popularPathsR user repo =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "popular", "paths"] []

-- | The total number of views over the last 14 days, and a daily or weekly breakdown.
--
-- > views' (OAuth "supersecrettoken") "qfpl" "tasty-hedgehog" Day
views' :: Auth -> Name Owner -> Name Repo -> Period p -> IO (Either Error (Views p))
views' auth user repo =
    executeRequest auth . viewsR user repo

-- | The total number of views over the last 14 days, and a daily or weekly breakdown.
-- See <https://developer.github.com/v3/repos/traffic/#views>
viewsR :: Name Owner -> Name Repo -> Period p -> Request k (Views p)
viewsR user repo period =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "views"] [("per", Just $ prettyPeriod period)]

-- | The total number of clones over the last 14 days, and a daily or weekly breakdown.
--
-- > clones' (OAuth "supersecrettoken") "qfpl" "tasty-hedgehog" Week
clones' :: Auth -> Name Owner -> Name Repo -> Period p -> IO (Either Error (Clones p))
clones' auth user repo =
    executeRequest auth . clonesR user repo

-- | The total number of clones over the last 14 days, and a daily or weekly breakdown.
-- See <https://developer.github.com/v3/repos/traffic/#clones>
clonesR :: Name Owner -> Name Repo -> Period p -> Request k (Clones p)
clonesR user repo period =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "clones"] [("per", Just $ prettyPeriod period)]
