{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Content where

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

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
  ,contentUrl     :: !Text
  ,contentGitUrl  :: !Text
  ,contentHtmlUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo where rnf = genericRnf
instance Binary ContentInfo
