{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Github.Data.Teams where

import Github.Data.Definitions

import Control.DeepSeq (NFData)
import Data.Data       (Data, Typeable)
import Data.Text       (Text)
import GHC.Generics    (Generic)

import Github.Data.Id
import Github.Data.Name

data Privacy =
    PrivacyClosed
  | PrivacySecret
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Privacy

data Permission =
    PermissionPull
  | PermissionPush
  | PermissionAdmin
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Permission

data Team = Team {
   teamId              :: !(Id Team)
  ,teamUrl             :: !Text
  ,teamName            :: !Text
  ,teamSlug            :: !(Name Team)
  ,teamDescription     :: !(Maybe Text)
  ,teamPrivacy         :: !(Maybe Privacy)
  ,teamPermission      :: !Permission
  ,teamMembersUrl      :: !Text
  ,teamRepositoriesUrl :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Team

data DetailedTeam = DetailedTeam {
   detailedTeamId              :: !(Id Team)
  ,detailedTeamUrl             :: !Text
  ,detailedTeamName            :: !(Name Team)
  ,detailedTeamSlug            :: !Text
  ,detailedTeamDescription     :: !(Maybe Text)
  ,detailedTeamPrivacy         :: !(Maybe Privacy)
  ,detailedTeamPermission      :: !Permission
  ,detailedTeamMembersUrl      :: !Text
  ,detailedTeamRepositoriesUrl :: !Text
  ,detailedTeamMembersCount    :: !Int
  ,detailedTeamReposCount      :: !Int
  ,detailedTeamOrganization    :: !GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedTeam

data CreateTeam = CreateTeam {
   createTeamName        :: !(Name Team)
  ,createTeamDescription :: !(Maybe Text)
  ,createRepoNames       :: ![Text]
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

instance NFData EditTeam

data Role =
     RoleMaintainer
  |  RoleMember
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Role

data ReqState =
     StatePending
  |  StateActive
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ReqState

data TeamMembership = TeamMembership {
  teamMembershipUrl      :: !Text,
  teamMembershipRole     :: !Role,
  teamMembershipReqState :: !ReqState
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData TeamMembership

data CreateTeamMembership = CreateTeamMembership {
  createTeamMembershipRole :: !Role
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeamMembership
