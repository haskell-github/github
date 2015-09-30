{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Github.Data.Teams where

import Github.Data.Definitions

import Control.DeepSeq (NFData)
import Data.Data (Typeable, Data)
import GHC.Generics (Generic)

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
   teamId :: Int
  ,teamUrl :: String
  ,teamName :: String
  ,teamSlug :: String
  ,teamDescription :: Maybe String
  ,teamPrivacy :: Maybe Privacy
  ,teamPermission :: Permission
  ,teamMembersUrl :: String
  ,teamRepositoriesUrl :: String
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Team

data DetailedTeam = DetailedTeam {
   detailedTeamId :: Int
  ,detailedTeamUrl :: String
  ,detailedTeamName :: String
  ,detailedTeamSlug :: String
  ,detailedTeamDescription :: Maybe String
  ,detailedTeamPrivacy :: Maybe Privacy
  ,detailedTeamPermission :: Permission
  ,detailedTeamMembersUrl :: String
  ,detailedTeamRepositoriesUrl :: String
  ,detailedTeamMembersCount :: Int
  ,detailedTeamReposCount :: Int
  ,detailedTeamOrganization :: GithubOwner
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DetailedTeam

data CreateTeam = CreateTeam {
   createTeamName :: String
  ,createTeamDescription :: Maybe String
  ,createRepoNames :: [String]
  {-,createTeamPrivacy :: Privacy-}
  ,createTeamPermission :: Permission
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeam

data EditTeam = EditTeam {
   editTeamName :: String
  ,editTeamDescription :: Maybe String
  {-,editTeamPrivacy :: Privacy-}
  ,editTeamPermission :: Permission
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
  teamMembershipUrl :: String,
  teamMembershipRole :: Role,
  teamMembershipReqState :: ReqState
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData TeamMembership

data CreateTeamMembership = CreateTeamMembership {
  createTeamMembershipRole :: Role
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeamMembership