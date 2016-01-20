{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Comments where

import Prelude        ()
import Prelude.Compat

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), ToJSON (..), object, withObject,
                                 (.:), (.:?), (.=))
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

import Github.Data.Definitions
import Github.Data.Id

data Comment = Comment {
   commentPosition  :: !(Maybe Int)
  ,commentLine      :: !(Maybe Int)
  ,commentBody      :: !Text
  ,commentCommitId  :: !(Maybe Text)
  ,commentUpdatedAt :: !UTCTime
  ,commentHtmlUrl   :: !(Maybe Text)
  ,commentUrl       :: !Text
  ,commentCreatedAt :: !(Maybe UTCTime)
  ,commentPath      :: !(Maybe Text)
  ,commentUser      :: !SimpleUser
  ,commentId        :: !(Id Comment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Comment where rnf = genericRnf
instance Binary Comment

instance FromJSON Comment where
    parseJSON = withObject "Comment" $ \o -> Comment
        <$> o .:? "position"
        <*> o .:? "line"
        <*> o .: "body"
        <*> o .:? "commit_id"
        <*> o .: "updated_at"
        <*> o .:? "html_url"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .:? "path"
        <*> o .: "user"
        <*> o .: "id"

data NewComment = NewComment {
   newCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewComment where rnf = genericRnf
instance Binary NewComment

instance ToJSON NewComment where
    toJSON (NewComment b) = object [ "body" .= b ]

data EditComment = EditComment {
   editCommentBody :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditComment where rnf = genericRnf
instance Binary EditComment

instance ToJSON EditComment where
    toJSON (EditComment b) = object [ "body" .= b ]
