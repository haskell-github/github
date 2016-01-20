{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Gists where

import Prelude        ()
import Prelude.Compat

import Github.Data.Definitions
import Github.Data.Id          (Id)
import Github.Data.Name        (Name)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), withObject, (.:), (.:?))
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.HashMap.Strict      (HashMap)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)

data Gist = Gist {
   gistUser        :: !SimpleUser
  ,gistGitPushUrl  :: !Text
  ,gistUrl         :: !Text
  ,gistDescription :: !(Maybe Text)
  ,gistCreatedAt   :: !UTCTime
  ,gistPublic      :: !Bool
  ,gistComments    :: !Int
  ,gistUpdatedAt   :: !UTCTime
  ,gistHtmlUrl     :: !Text
  ,gistId          :: !(Name Gist)
  ,gistFiles       :: !(HashMap Text GistFile)
  ,gistGitPullUrl  :: !Text
} deriving (Show, Data, Typeable, Eq, Generic)

instance NFData Gist where rnf = genericRnf
instance Binary Gist

instance FromJSON Gist where
  parseJSON = withObject "Gist" $ \o ->
    Gist <$> o .: "owner"
         <*> o .: "git_push_url"
         <*> o .: "url"
         <*> o .:? "description"
         <*> o .: "created_at"
         <*> o .: "public"
         <*> o .: "comments"
         <*> o .: "updated_at"
         <*> o .: "html_url"
         <*> o .: "id"
         <*> o .: "files"
         <*> o .: "git_push_url"

data GistFile = GistFile {
   gistFileType     :: !Text
  ,gistFileRawUrl   :: !Text
  ,gistFileSize     :: !Int
  ,gistFileLanguage :: !(Maybe Text)
  ,gistFileFilename :: !Text
  ,gistFileContent  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Generic)

instance NFData GistFile where rnf = genericRnf
instance Binary GistFile

instance FromJSON GistFile where
  parseJSON = withObject "GistFile" $ \o ->
    GistFile <$> o .: "type"
             <*> o .: "raw_url"
             <*> o .: "size"
             <*> o .:? "language"
             <*> o .: "filename"
             <*> o .:? "content"

data GistComment = GistComment {
   gistCommentUser      :: !SimpleUser
  ,gistCommentUrl       :: !Text
  ,gistCommentCreatedAt :: !UTCTime
  ,gistCommentBody      :: !Text
  ,gistCommentUpdatedAt :: !UTCTime
  ,gistCommentId        :: !(Id GistComment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment where rnf = genericRnf
instance Binary GistComment

instance FromJSON GistComment where
  parseJSON = withObject "GistComment" $ \o ->
    GistComment <$> o .: "user"
                <*> o .: "url"
                <*> o .: "created_at"
                <*> o .: "body"
                <*> o .: "updated_at"
                <*> o .: "id"
