-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GitHub.Data.Projects where

import GitHub.Data.Definitions
import GitHub.Data.Name
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import Data.Tagged (Tagged (..))
-- import qualified GitHub.Request as GH

import qualified Data.Text as T

data ProjectState = ProjectStateOpen | ProjectStateClosed
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ProjectState where rnf = genericRnf
instance Binary ProjectState

instance FromJSON ProjectState where
  parseJSON = withText "ProjecState" $ \t -> case T.toLower t of
    "open" -> pure ProjectStateOpen
    "closed" -> pure ProjectStateClosed
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
  , projectCreator :: !SimpleUser
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


data Column = Column
  {
    columnUrl :: !URL,
    columnProjectUrl :: !URL,
    columnCardsUrl :: !URL,
    columnId :: !(Id Column),
    columnName :: !(Name Column),
    columnCreatedAt :: !UTCTime,
    columntUpdatedAt :: !UTCTime
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Column where rnf = genericRnf

instance Binary Column

instance FromJSON Column where
  parseJSON = withObject "Column" $ \o ->
    Column
      <$> o .: "url"
      <*> o .: "project_url"
      <*> o .: "cards_url"
      <*> o .: "id"
      <*> o .: "name"
      <*> o .: "created_at"
      <*> o .: "updated_at"


data Card = Card
  { cardUrl :: !URL,
    cardId :: !(Id Column),
    cardNote:: !(Maybe T.Text),
    cardCreator:: !(SimpleUser),
    cardCreatedAt :: !UTCTime,
    cardUpdatedAt :: !UTCTime,
    archived:: !Bool,
    cardColumnUrl:: !URL,
    cardContentUrl:: !(Maybe URL),
    cardProjectUrl:: !URL
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Card where rnf = genericRnf

instance Binary Card

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card
      <$> o .: "url"
      <*> o .: "id"
      <*> o .:? "note"
      <*> o .: "creator"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .: "archived"
      <*> o .: "column_url"
      <*> o .:? "content_url"
      <*> o .: "project_url"
