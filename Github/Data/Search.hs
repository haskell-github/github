{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Search where

import Github.Data.Repos (Repo)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

data SearchResult entity = SearchResult {
   searchResultTotalCount :: !Int
  ,searchResultResults    :: !(Vector entity)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData entity => NFData (SearchResult entity) where rnf = genericRnf
instance Binary entity => Binary (SearchResult entity)

data Code = Code {
   codeName    :: !Text
  ,codePath    :: !Text
  ,codeSha     :: !Text
  ,codeUrl     :: !Text
  ,codeGitUrl  :: !Text
  ,codeHtmlUrl :: !Text
  ,codeRepo    :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Code where rnf = genericRnf
instance Binary Code
