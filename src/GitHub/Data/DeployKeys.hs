-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
module GitHub.Data.DeployKeys where

import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude

data RepoDeployKey = RepoDeployKey {
   repoDeployKeyId           :: !(Id RepoDeployKey)
  ,repoDeployKeyKey          :: !Text
  ,repoDeployKeyUrl          :: !URL
  ,repoDeployKeyTitle        :: !Text
  ,repoDeployKeyVerified     :: !Bool
  ,repoDeployKeyCreatedAt    :: !UTCTime
  ,repoDeployKeyReadOnly     :: !Bool
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON RepoDeployKey where
  parseJSON = withObject "RepoDeployKey" $ \o ->
    RepoDeployKey <$> o .: "id"
                  <*> o .: "key"
                  <*> o .: "url"
                  <*> o .: "title"
                  <*> o .: "verified"
                  <*> o .: "created_at"
                  <*> o .: "read_only"
