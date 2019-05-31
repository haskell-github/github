-- | The traffic API, as described at <https://developer.github.com/v3/repos/traffic/>
module GitHub.Endpoints.Repos.Traffic (
    popularReferrers,
    popularReferrers',
    popularReferrersR
    ) where

import Data.Vector (Vector)

import GitHub.Data (Referrer, Name, Repo, Owner, Auth, Error, Clones, Path, Period, Views)
import GitHub.Data.Request (query, toPathPart)
import GitHub.Request (Request, executeRequestMaybe)

-- | The top 10 referrers for the past 14 days.
--
-- > popularReferrers "qfpl" "tasty-hedgehog"
popularReferrers :: Name Owner -> Name Repo -> IO (Either Error (Vector Referrer))
popularReferrers =
  popularReferrers' Nothing

-- | The top 10 referrers for the past 14 days.
-- | With authentication.
--
-- > popularReferrers' (Just $ BasicAuth "github-username" "github-password") "qfpl" "tasty-hedgehog"
popularReferrers' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Referrer))
popularReferrers' auth user repo =
  executeRequestMaybe auth $ popularReferrersR user repo

popularReferrersR :: Name Owner -> Name Repo -> Request k (Vector Referrer)
popularReferrersR user repo =
  query ["repos", toPathPart user, toPathPart repo, "traffic", "popular", "referrers"] []

popularPaths :: Name Owner -> Name Repo -> IO (Either Error (Vector Path))
popularPaths =
  undefined

popularPaths' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Path))
popularPaths' =
  undefined

views :: Name Owner -> Name Repo -> Period -> IO (Either Error Views)
views =
  undefined

views' :: Maybe Auth -> Name Owner -> Name Repo -> Period -> IO (Either Error Views)
views' =
  undefined

clones :: Name Owner -> Name Repo -> Period -> IO (Either Error Clones)
clones =
  undefined

clones' :: Maybe Auth -> Name Owner -> Name Repo -> Period -> IO (Either Error Clones)
clones' =
  undefined