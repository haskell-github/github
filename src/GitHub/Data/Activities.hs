-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Activities where

import GitHub.Data.Repos       (Repo)
import GitHub.Internal.Prelude
import Prelude ()

data RepoStarred = RepoStarred
    { repoStarredStarredAt :: !UTCTime
    , repoStarredRepo      :: !Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoStarred where rnf = genericRnf
instance Binary RepoStarred

-- JSON Instances
instance FromJSON RepoStarred where
    parseJSON = withObject "RepoStarred" $ \o -> RepoStarred
        <$> o .: "starred_at"
        <*> o .: "repo"

