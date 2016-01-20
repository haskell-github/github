{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Gists where

import Github.Data.Definitions
import Github.Data.Id          (Id)
import Github.Data.Name        (Name)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

data Gist = Gist {
   gistUser        :: !SimpleOwner
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
instance Binary Gist

data GistFile = GistFile {
   gistFileType     :: !Text
  ,gistFileRawUrl   :: !Text
  ,gistFileSize     :: !Int
  ,gistFileLanguage :: !(Maybe Text)
  ,gistFileFilename :: !Text
  ,gistFileContent  :: !(Maybe Text)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistFile where rnf = genericRnf
instance Binary GistFile

data GistComment = GistComment {
   gistCommentUser      :: !SimpleOwner
  ,gistCommentUrl       :: !Text
  ,gistCommentCreatedAt :: !UTCTime
  ,gistCommentBody      :: !Text
  ,gistCommentUpdatedAt :: !UTCTime
  ,gistCommentId        :: !(Id GistComment)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment where rnf = genericRnf
instance Binary GistComment
