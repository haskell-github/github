{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Teams where

import Github.Data.Definitions

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Vector              (Vector)
import GHC.Generics             (Generic)

import Github.Data.Id    (Id)
import Github.Data.Name  (Name)
import Github.Data.Repos (Repo)

data Privacy =
    PrivacyClosed
  | PrivacySecret
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Privacy where rnf = genericRnf

data Permission =
    PermissionPull
  | PermissionPush
  | PermissionAdmin
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Permission where rnf = genericRnf

data SimpleTeam = SimpleTeam {
   simpleTeamId              :: !(Id Team)
  ,simpleTeamUrl             :: !Text
  ,simpleTeamName            :: !Text
  ,simpleTeamSlug            :: !(Name Team)
  ,simpleTeamDescription     :: !(Maybe Text)
  ,simpleTeamPrivacy         :: !(Maybe Privacy)
  ,simpleTeamPermission      :: !Permission
  ,simpleTeamMembersUrl      :: !Text
  ,simpleTeamRepositoriesUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleTeam where rnf = genericRnf

data Team = Team {
   teamId              :: !(Id Team)
  ,teamUrl             :: !Text
  ,teamName            :: !(Name Team)
  ,teamSlug            :: !Text
  ,teamDescription     :: !(Maybe Text)
  ,teamPrivacy         :: !(Maybe Privacy)
  ,teamPermission      :: !Permission
  ,teamMembersUrl      :: !Text
  ,teamRepositoriesUrl :: !Text
  ,teamMembersCount    :: !Int
  ,teamReposCount      :: !Int
  ,teamOrganization    :: !SimpleOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Team where rnf = genericRnf

data CreateTeam = CreateTeam {
   createTeamName        :: !(Name Team)
  ,createTeamDescription :: !(Maybe Text)
  ,createTeamRepoNames   :: !(Vector (Name Repo))
  {-,createTeamPrivacy :: Privacy-}
  ,createTeamPermission  :: Permission
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeam

data EditTeam = EditTeam {
   editTeamName        :: !(Name Team)
  ,editTeamDescription :: !(Maybe Text)
  {-,editTeamPrivacy :: Privacy-}
  ,editTeamPermission  :: !Permission
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditTeam where rnf = genericRnf

data Role =
     RoleMaintainer
  |  RoleMember
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Role

data ReqState =
     StatePending
  |  StateActive
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ReqState where rnf = genericRnf

data TeamMembership = TeamMembership {
  teamMembershipUrl      :: !Text,
  teamMembershipRole     :: !Role,
  teamMembershipReqState :: !ReqState
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData TeamMembership where rnf = genericRnf

data CreateTeamMembership = CreateTeamMembership {
  createTeamMembershipRole :: !Role
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeamMembership where rnf = genericRnf
