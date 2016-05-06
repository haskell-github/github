{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Activities where

import Prelude        ()
import Prelude.Compat

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), withObject, (.:))
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

import GitHub.Data.Repos (Repo)

data RepoStarred = RepoStarred {
   repoStarredStarredAt :: !UTCTime
  ,repoStarredRepo      :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoStarred where rnf = genericRnf
instance Binary RepoStarred

-- JSON Instances
instance FromJSON RepoStarred where
    parseJSON = withObject "RepoStarred" $ \o -> RepoStarred
        <$> o .: "starred_at"
        <*> o .: "repo"

