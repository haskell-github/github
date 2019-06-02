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

import GitHub.Data (Referrer, Name, Repo, Owner, Auth, Error, Clones, PopularPath, Period, Views, prettyPeriod)
import GitHub.Data.Request (query, toPathPart)
import GitHub.Request (Request, executeRequest)

-- | The top 10 referrers for the past 14 days.
-- | With authentication.
--
-- > popularReferrers' (Just $ BasicAuth "github-username" "github-password") "qfpl" "tasty-hedgehog"
popularReferrers' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Referrer))
popularReferrers' auth user =
  executeRequest auth . popularReferrersR user

popularReferrersR :: Name Owner -> Name Repo -> Request k (Vector Referrer)
popularReferrersR user repo =
  query ["repos", toPathPart user, toPathPart repo, "traffic", "popular", "referrers"] []

popularPaths' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector PopularPath))
popularPaths' auth user =
  executeRequest auth . popularPathsR user

popularPathsR :: Name Owner -> Name Repo -> Request k (Vector PopularPath)
popularPathsR user repo =
  query ["repos", toPathPart user, toPathPart repo, "traffic", "popular", "paths"] []

views' :: Auth -> Name Owner -> Name Repo -> Period p -> IO (Either Error (Views p))
views' auth user repo =
  executeRequest auth . viewsR user repo

viewsR :: Name Owner -> Name Repo -> Period p -> Request k (Views p)
viewsR user repo period =
  query ["repos", toPathPart user, toPathPart repo, "traffic", "views"] [("per", Just $ prettyPeriod period)]

clones' :: Auth -> Name Owner -> Name Repo -> Period p -> IO (Either Error (Clones p))
clones' auth user repo =
  executeRequest auth . clonesR user repo

clonesR :: Name Owner -> Name Repo -> Period p -> Request k (Clones p)
clonesR user repo period =
  query ["repos", toPathPart user, toPathPart repo, "traffic", "clones"] [("per", Just $ prettyPeriod period)]
