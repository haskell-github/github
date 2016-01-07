{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Search where

import Github.Data.Repos (Repo)
import Github.Data.Issues (Issue)

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import GHC.Generics    (Generic)

data SearchReposResult = SearchReposResult {
   searchReposTotalCount :: !Int
  ,searchReposRepos     :: ![Repo]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchReposResult

data Code = Code {
   codeName    :: !Text
  ,codePath    :: !Text
  ,codeSha     :: !Text
  ,codeUrl     :: !Text
  ,codeGitUrl  :: !Text
  ,codeHtmlUrl :: !Text
  ,codeRepo    :: !Repo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Code

data SearchCodeResult = SearchCodeResult {
   searchCodeTotalCount :: !Int
  ,searchCodeCodes      :: ![Code]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchCodeResult

data SearchIssuesResult = SearchIssuesResult {
   searchIssuesTotalCount :: Int
  ,searchIssuesIssues     :: [Issue]
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SearchIssuesResult
