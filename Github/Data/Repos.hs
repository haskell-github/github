{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Repos where

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

data Repo = Repo {
   repoSshUrl          :: !(Maybe Text)
  ,repoDescription     :: !(Maybe Text)
  ,repoCreatedAt       :: !(Maybe UTCTime)
  ,repoHtmlUrl         :: !Text
  ,repoSvnUrl          :: !(Maybe Text)
  ,repoForks           :: !(Maybe Int)
  ,repoHomepage        :: !(Maybe Text)
  ,repoFork            :: !(Maybe Bool)
  ,repoGitUrl          :: !(Maybe Text)
  ,repoPrivate         :: !Bool
  ,repoCloneUrl        :: !(Maybe Text)
  ,repoSize            :: !(Maybe Int)
  ,repoUpdatedAt       :: !(Maybe UTCTime)
  ,repoWatchers        :: !(Maybe Int)
  ,repoOwner           :: !SimpleOwner
  ,repoName            :: !(Name Repo)
  ,repoLanguage        :: !(Maybe Text)
  ,repoMasterBranch    :: !(Maybe Text)
  ,repoPushedAt        :: !(Maybe UTCTime)   -- ^ this is Nothing for new repositories
  ,repoId              :: !(Id Repo)
  ,repoUrl             :: !Text
  ,repoOpenIssues      :: !(Maybe Int)
  ,repoHasWiki         :: !(Maybe Bool)
  ,repoHasIssues       :: !(Maybe Bool)
  ,repoHasDownloads    :: !(Maybe Bool)
  ,repoParent          :: !(Maybe RepoRef)
  ,repoSource          :: !(Maybe RepoRef)
  ,repoHooksUrl        :: !Text
  ,repoStargazersCount :: !Int
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Repo where rnf = genericRnf
instance Binary Repo

data RepoRef = RepoRef SimpleOwner (Name Repo) -- Repo owner and name
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoRef where rnf = genericRnf
instance Binary RepoRef

data NewRepo = NewRepo {
  newRepoName        :: !(Name Repo)
, newRepoDescription :: !(Maybe Text)
, newRepoHomepage    :: !(Maybe Text)
, newRepoPrivate     :: !(Maybe Bool)
, newRepoHasIssues   :: !(Maybe Bool)
, newRepoHasWiki     :: !(Maybe Bool)
, newRepoAutoInit    :: !(Maybe Bool)
} deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData NewRepo where rnf = genericRnf
instance Binary NewRepo

newRepo :: Name Repo -> NewRepo
newRepo name = NewRepo name Nothing Nothing Nothing Nothing Nothing Nothing

data EditRepo = EditRepo {
  editName         :: !(Maybe (Name Repo))
, editDescription  :: !(Maybe Text)
, editHomepage     :: !(Maybe Text)
, editPublic       :: !(Maybe Bool)
, editHasIssues    :: !(Maybe Bool)
, editHasWiki      :: !(Maybe Bool)
, editHasDownloads :: !(Maybe Bool)
} deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData EditRepo where rnf = genericRnf
instance Binary EditRepo

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity =
    All     -- ^ All repos accessible to the user.
  | Owner   -- ^ Only repos owned by the user.
  | Public  -- ^ Only public repos.
  | Private -- ^ Only private repos.
  | Member  -- ^ Only repos to which the user is a member but not an owner.
 deriving (Show, Eq, Ord, Typeable, Data, Generic)

-- | This is only used for the FromJSON instance.
data Languages = Languages { getLanguages :: Vector Language }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Languages where rnf = genericRnf
instance Binary Languages

-- | A programming language with the name and number of characters written in
-- it.
data Language = Language !Text !Int
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Language where rnf = genericRnf
instance Binary Language
