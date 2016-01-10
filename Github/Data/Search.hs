{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Search where

import Github.Data.Issues (Issue)
import Github.Data.Repos  (Repo)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import Data.Vector     (Vector)
import GHC.Generics    (Generic)

data SearchReposResult = SearchReposResult {
   searchReposTotalCount :: !Int
  ,searchReposRepos      :: !(Vector Repo)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchReposResult where rnf = genericRnf

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

data SearchCodeResult = SearchCodeResult {
   searchCodeTotalCount :: !Int
  ,searchCodeCodes      :: !(Vector Code)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchCodeResult where rnf = genericRnf

data SearchIssuesResult = SearchIssuesResult {
   searchIssuesTotalCount :: !Int
  ,searchIssuesIssues     :: !(Vector Issue)
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchIssuesResult where rnf = genericRnf
