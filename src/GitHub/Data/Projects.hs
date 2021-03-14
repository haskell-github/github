-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Projects where

import GitHub.Data.Definitions
import GitHub.Data.Name
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T

data ProjectState = StateOpen | StateClosed
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ProjectState where rnf = genericRnf
instance Binary ProjectState

instance FromJSON ProjectState where
  parseJSON = withText "ProjecState" $ \t -> case T.toLower t of
    "open" -> pure StateOpen
    "closed" -> pure StateClosed
    _ -> fail $ "Unknown ProjectState: " <> T.unpack t

data Project = Project
  {
    projectOwnerUrl:: !URL
  , projectUrl:: !URL
  , projectHtmlUrl:: !URL
  , projectColumnsUrl:: !URL
  , projectId :: !(Id Project)
  , projectName :: !(Name Project)
  , projectBody :: !(Maybe Text)
  , projectNumber :: !Int
  , projectState :: !ProjectState
  , projectCreator :: !User
  , projectCreatedAt :: !UTCTime
  , projectUpdatedAt :: !UTCTime
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Project where rnf = genericRnf
instance Binary Project

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o -> Project
        <$> o .: "owner_url"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "columns_url"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .:? "body"
        <*> o .: "number"
        <*> o .: "state"
        <*> o .: "creator"
        <*> o .: "created_at"
        <*> o .: "updated_at"
