{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Gists where

import Github.Data.Definitions
import Github.Data.Id
import Github.Data.Name

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import GHC.Generics    (Generic)

data Gist = Gist {
   gistUser        :: !GithubOwner
  ,gistGitPushUrl  :: !Text
  ,gistUrl         :: !Text
  ,gistDescription :: !(Maybe Text)
  ,gistCreatedAt   :: !GithubDate
  ,gistPublic      :: !Bool
  ,gistComments    :: !Int
  ,gistUpdatedAt   :: !GithubDate
  ,gistHtmlUrl     :: !Text
  ,gistId          :: !(Name Gist)
  ,gistFiles       :: ![GistFile]
  ,gistGitPullUrl  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Gist

data GistFile = GistFile {
   gistFileType     :: !Text
  ,gistFileRawUrl   :: !Text
  ,gistFileSize     :: !Int
  ,gistFileLanguage :: !(Maybe Text)
  ,gistFileFilename :: !Text
  ,gistFileContent  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistFile

data GistComment = GistComment {
   gistCommentUser      :: !GithubOwner
  ,gistCommentUrl       :: !Text
  ,gistCommentCreatedAt :: !GithubDate
  ,gistCommentBody      :: !Text
  ,gistCommentUpdatedAt :: !GithubDate
  ,gistCommentId        :: !(Id GistComment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment
