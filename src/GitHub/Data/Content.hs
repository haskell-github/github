{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Content where

import Data.Aeson.Types        (Pair)
import Data.Maybe              (maybe)
import GitHub.Data.GitData
import GitHub.Data.URL
import GitHub.Internal.Prelude
import Prelude ()

data Content
  = ContentFile !ContentFileData
  | ContentDirectory !(Vector ContentItem)
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content where rnf = genericRnf
instance Binary Content

data ContentFileData = ContentFileData {
   contentFileInfo     :: !ContentInfo
  ,contentFileEncoding :: !Text
  ,contentFileSize     :: !Int
  ,contentFileContent  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentFileData where rnf = genericRnf
instance Binary ContentFileData

-- | An item in a directory listing.
data ContentItem = ContentItem {
   contentItemType :: !ContentItemType
  ,contentItemInfo :: !ContentInfo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItem where rnf = genericRnf
instance Binary ContentItem

data ContentItemType = ItemFile | ItemDir
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItemType where rnf = genericRnf
instance Binary ContentItemType

-- | Information common to both kinds of Content: files and directories.
data ContentInfo = ContentInfo {
   contentName    :: !Text
  ,contentPath    :: !Text
  ,contentSha     :: !Text
  ,contentUrl     :: !URL
  ,contentGitUrl  :: !URL
  ,contentHtmlUrl :: !URL
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo where rnf = genericRnf
instance Binary ContentInfo

data ContentResultInfo = ContentResultInfo
    { contentResultInfo :: !ContentInfo
    , contentResultSize :: !Int
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentResultInfo where rnf = genericRnf
instance Binary ContentResultInfo

data ContentResult = ContentResult
    { contentResultContent  :: !ContentResultInfo
    , contentResultCommit   :: !GitCommit
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentResult where rnf = genericRnf
instance Binary ContentResult

data Author = Author
    { authorName  :: !Text
    , authorEmail :: !Text
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Author where rnf = genericRnf
instance Binary Author

data CreateFile = CreateFile
    { createFilePath      :: !Text
    , createFileMessage   :: !Text
    , createFileContent   :: !Text
    , createFileBranch    :: !(Maybe Text)
    , createFileAuthor    :: !(Maybe Author)
    , createFileCommitter :: !(Maybe Author)
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData CreateFile where rnf = genericRnf
instance Binary CreateFile

data UpdateFile = UpdateFile
    { updateFilePath      :: !Text
    , updateFileMessage   :: !Text
    , updateFileContent   :: !Text
    , updateFileSHA       :: !Text
    , updateFileBranch    :: !(Maybe Text)
    , updateFileAuthor    :: !(Maybe Author)
    , updateFileCommitter :: !(Maybe Author)
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData UpdateFile where rnf = genericRnf
instance Binary UpdateFile

data DeleteFile = DeleteFile
    { deleteFilePath      :: !Text
    , deleteFileMessage   :: !Text
    , deleteFileSHA       :: !Text
    , deleteFileBranch    :: !(Maybe Text)
    , deleteFileAuthor    :: !(Maybe Author)
    , deleteFileCommitter :: !(Maybe Author)
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData DeleteFile where rnf = genericRnf
instance Binary DeleteFile

instance FromJSON Content where
  parseJSON o@(Object _) = ContentFile <$> parseJSON o
  parseJSON (Array os) = ContentDirectory <$> traverse parseJSON os
  parseJSON _ = fail "Could not build a Content"

instance FromJSON ContentFileData where
  parseJSON = withObject "ContentFileData" $ \o ->
    ContentFileData <$> parseJSON (Object o)
                    <*> o .: "encoding"
                    <*> o .: "size"
                    <*> o .: "content"

instance FromJSON ContentItem where
  parseJSON = withObject "ContentItem" $ \o ->
    ContentItem <$> o .: "type"
                <*> parseJSON (Object o)

instance FromJSON ContentItemType where
  parseJSON = withText "ContentItemType" $ \t ->
      case t of
          "file" -> return ItemFile
          "dir"  -> return ItemDir
          _      -> fail $ "Invalid ContentItemType: " ++ unpack t

instance FromJSON ContentInfo where
  parseJSON = withObject "ContentInfo" $ \o ->
    ContentInfo <$> o .: "name"
                <*> o .: "path"
                <*> o .: "sha"
                <*> o .: "url"
                <*> o .: "git_url"
                <*> o .: "html_url"

instance FromJSON ContentResultInfo where
  parseJSON = withObject "ContentResultInfo" $ \o ->
    ContentResultInfo <$> parseJSON (Object o)
                  <*> o .: "size"

instance FromJSON ContentResult where
  parseJSON = withObject "ContentResult" $ \o ->
    ContentResult <$> o .: "content"
                  <*> o .: "commit"

instance ToJSON Author where
  toJSON Author {..} = object
    [ "name"  .= authorName
    , "email" .= authorEmail
    ]

instance ToJSON CreateFile where
  toJSON CreateFile {..} = object $
    [ "path"       .= createFilePath
    , "message"    .= createFileMessage
    , "content"    .= createFileContent
    ]
    ++ "branch"    .=? createFileBranch
    ++ "author"    .=? createFileAuthor
    ++ "committer" .=? createFileCommitter

instance ToJSON UpdateFile where
  toJSON UpdateFile {..} = object $
    [ "path"       .= updateFilePath
    , "message"    .= updateFileMessage
    , "content"    .= updateFileContent
    , "sha"        .= updateFileSHA
    ]
    ++ "branch"    .=? updateFileBranch
    ++ "author"    .=? updateFileAuthor
    ++ "committer" .=? updateFileCommitter

instance ToJSON DeleteFile where
  toJSON DeleteFile {..} = object $
    [ "path"       .= deleteFilePath
    , "message"    .= deleteFileMessage
    , "sha"        .= deleteFileSHA
    ]
    ++ "branch"    .=? deleteFileBranch
    ++ "author"    .=? deleteFileAuthor
    ++ "committer" .=? deleteFileCommitter

(.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
name .=? value = maybe [] (pure . (name .=)) value
