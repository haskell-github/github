{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Gists where

import Github.Data.Definitions
import Github.Data.Id          (Id)
import Github.Data.Name        (Name)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

data Gist = Gist {
   gistUser        :: !GithubOwner
  ,gistGitPushUrl  :: !Text
  ,gistUrl         :: !Text
  ,gistDescription :: !(Maybe Text)
  ,gistCreatedAt   :: !UTCTime
  ,gistPublic      :: !Bool
  ,gistComments    :: !Int
  ,gistUpdatedAt   :: !UTCTime
  ,gistHtmlUrl     :: !Text
  ,gistId          :: !(Name Gist)
  ,gistFiles       :: !(Vector GistFile)
  ,gistGitPullUrl  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Gist where rnf = genericRnf

data GistFile = GistFile {
   gistFileType     :: !Text
  ,gistFileRawUrl   :: !Text
  ,gistFileSize     :: !Int
  ,gistFileLanguage :: !(Maybe Text)
  ,gistFileFilename :: !Text
  ,gistFileContent  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistFile where rnf = genericRnf

data GistComment = GistComment {
   gistCommentUser      :: !GithubOwner
  ,gistCommentUrl       :: !Text
  ,gistCommentCreatedAt :: !UTCTime
  ,gistCommentBody      :: !Text
  ,gistCommentUpdatedAt :: !UTCTime
  ,gistCommentId        :: !(Id GistComment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment where rnf = genericRnf
