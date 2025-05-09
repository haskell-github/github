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
       (Auth, Clones, Error, Name, Owner, Period (Day, Week), PopularPath,
       Referrer, Repo, Views)
import GitHub.Data.Request     (query, toPathPart)
import GitHub.Internal.Prelude
import GitHub.Request          (Request, executeRequest)
import Prelude ()

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
views' :: Auth -> Name Owner -> Name Repo -> Period -> IO (Either Error Views)
views' auth user repo =
    executeRequest auth . viewsR user repo

-- | The total number of views over the last 14 days, and a daily or weekly breakdown.
-- See <https://developer.github.com/v3/repos/traffic/#views>
viewsR :: Name Owner -> Name Repo -> Period -> Request k Views
viewsR user repo period =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "views"]
          [("per", Just $ serializePeriod period)]

-- | The total number of clones over the last 14 days, and a daily or weekly breakdown.
--
-- > clones' (OAuth "supersecrettoken") "qfpl" "tasty-hedgehog" Week
clones' :: Auth -> Name Owner -> Name Repo -> Period -> IO (Either Error Clones)
clones' auth user repo =
    executeRequest auth . clonesR user repo

-- | The total number of clones over the last 14 days, and a daily or weekly breakdown.
-- See <https://developer.github.com/v3/repos/traffic/#clones>
clonesR :: Name Owner -> Name Repo -> Period -> Request k Clones
clonesR user repo period =
    query ["repos", toPathPart user, toPathPart repo, "traffic", "clones"]
          [("per", Just $ serializePeriod period)]

serializePeriod :: IsString a => Period -> a
serializePeriod p = case p of
    Day -> "day"
    Week -> "week"
