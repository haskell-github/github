{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Search where

import Prelude        ()
import Prelude.Compat

import Github.Data.Repos (Repo)

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

import qualified Data.Vector as V

data SearchResult entity = SearchResult {
   searchResultTotalCount :: !Int
  ,searchResultResults    :: !(Vector entity)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData entity => NFData (SearchResult entity) where rnf = genericRnf
instance Binary entity => Binary (SearchResult entity)

instance FromJSON entity => FromJSON (SearchResult entity) where
  parseJSON = withObject "SearchResult" $ \o ->
    SearchResult <$> o .: "total_count"
                 <*> o .:? "items" .!= V.empty

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

instance FromJSON Code where
  parseJSON = withObject "Code" $ \o ->
    Code <$> o .: "name"
         <*> o .: "path"
         <*> o .: "sha"
         <*> o .: "url"
         <*> o .: "git_url"
         <*> o .: "html_url"
         <*> o .: "repository"
