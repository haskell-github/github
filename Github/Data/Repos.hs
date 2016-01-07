{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Repos where

import Github.Data.Definitions
import Github.Data.Name

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import GHC.Generics    (Generic)

data NewRepo = NewRepo {
  newRepoName        :: !(Name Repo)
, newRepoDescription :: !(Maybe Text)
, newRepoHomepage    :: !(Maybe Text)
, newRepoPrivate     :: !(Maybe Bool)
, newRepoHasIssues   :: !(Maybe Bool)
, newRepoHasWiki     :: !(Maybe Bool)
, newRepoAutoInit    :: !(Maybe Bool)
} deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData NewRepo

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

instance NFData EditRepo

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity =
    All     -- ^ All repos accessible to the user.
  | Owner   -- ^ Only repos owned by the user.
  | Public  -- ^ Only public repos.
  | Private -- ^ Only private repos.
  | Member  -- ^ Only repos to which the user is a member but not an owner.
 deriving (Show, Eq, Ord, Typeable, Data, Generic)
