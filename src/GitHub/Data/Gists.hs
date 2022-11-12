-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Gists where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.Repos       (Language)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Gist = Gist
    { gistUser        :: !SimpleUser
    , gistGitPushUrl  :: !URL
    , gistUrl         :: !URL
    , gistDescription :: !(Maybe Text)
    , gistCreatedAt   :: !UTCTime
    , gistPublic      :: !Bool
    , gistComments    :: !Int
    , gistUpdatedAt   :: !UTCTime
    , gistHtmlUrl     :: !URL
    , gistId          :: !(Name Gist)
    , gistFiles       :: !(HashMap Text GistFile)
    , gistGitPullUrl  :: !URL
    } deriving (Show, Data, Typeable, Eq, Generic)

instance NFData Gist where rnf = genericRnf
instance Binary Gist

instance FromJSON Gist where
    parseJSON = withObject "Gist" $ \o -> Gist
        <$> o .: "owner"
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

data GistFile = GistFile
    { gistFileType     :: !Text
    , gistFileRawUrl   :: !URL
    , gistFileSize     :: !Int
    , gistFileLanguage :: !(Maybe Language)
    , gistFileFilename :: !Text
    , gistFileContent  :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Generic)

instance NFData GistFile where rnf = genericRnf
instance Binary GistFile

instance FromJSON GistFile where
    parseJSON = withObject "GistFile" $ \o -> GistFile
        <$> o .: "type"
        <*> o .: "raw_url"
        <*> o .: "size"
        <*> o .:? "language"
        <*> o .: "filename"
        <*> o .:? "content"

data GistComment = GistComment
    { gistCommentUser      :: !SimpleUser
    , gistCommentUrl       :: !URL
    , gistCommentCreatedAt :: !UTCTime
    , gistCommentBody      :: !Text
    , gistCommentUpdatedAt :: !UTCTime
    , gistCommentId        :: !(Id GistComment)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GistComment where rnf = genericRnf
instance Binary GistComment

instance FromJSON GistComment where
    parseJSON = withObject "GistComment" $ \o -> GistComment
        <$> o .: "user"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "body"
        <*> o .: "updated_at"
        <*> o .: "id"

data NewGist = NewGist
    { newGistDescription :: !(Maybe Text)
    , newGistFiles       :: !(HashMap Text NewGistFile)
    , newGistPublic      :: !(Maybe Bool)
    } deriving (Show, Data, Typeable, Eq, Generic)

instance NFData NewGist where rnf = genericRnf
instance Binary NewGist

instance ToJSON NewGist where
    toJSON NewGist { newGistDescription = description
                   , newGistFiles       = files
                   , newGistPublic      = public
                   } = object $ filter notNull
                   [ "description"      .= description
                   , "files"            .= files
                   , "public"           .= public
                   ]
      where
        notNull (_, Null) = False
        notNull (_, _)    = True

data NewGistFile = NewGistFile
    { newGistFileContent :: !Text
    } deriving (Show, Data, Typeable, Eq, Generic)

instance NFData NewGistFile where rnf = genericRnf
instance Binary NewGistFile

instance ToJSON NewGistFile where
    toJSON (NewGistFile c) = object ["content" .= c]
