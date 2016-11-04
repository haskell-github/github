{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Teams where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Privacy
    = PrivacyClosed
    | PrivacySecret
    deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData Privacy where rnf = genericRnf
instance Binary Privacy

data Permission
    = PermissionPull
    | PermissionPush
    | PermissionAdmin
    deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData Permission where rnf = genericRnf
instance Binary Permission

data AddTeamRepoPermission = AddTeamRepoPermission
    { addTeamRepoPermission :: !Permission
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData AddTeamRepoPermission where rnf = genericRnf
instance Binary AddTeamRepoPermission

data SimpleTeam = SimpleTeam
    { simpleTeamId              :: !(Id Team)
    , simpleTeamUrl             :: !URL
    , simpleTeamName            :: !Text  -- TODO (0.15.0): unify this and 'simpleTeamSlug' as in 'Team'.
    , simpleTeamSlug            :: !(Name Team)
    , simpleTeamDescription     :: !(Maybe Text)
    , simpleTeamPrivacy         :: !(Maybe Privacy)
    , simpleTeamPermission      :: !Permission
    , simpleTeamMembersUrl      :: !URL
    , simpleTeamRepositoriesUrl :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleTeam where rnf = genericRnf
instance Binary SimpleTeam

data Team = Team
    { teamId              :: !(Id Team)
    , teamUrl             :: !URL
    , teamName            :: !Text
    , teamSlug            :: !(Name Team)
    , teamDescription     :: !(Maybe Text)
    , teamPrivacy         :: !(Maybe Privacy)
    , teamPermission      :: !Permission
    , teamMembersUrl      :: !URL
    , teamRepositoriesUrl :: !URL
    , teamMembersCount    :: !Int
    , teamReposCount      :: !Int
    , teamOrganization    :: !SimpleOrganization
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Team where rnf = genericRnf
instance Binary Team

data CreateTeam = CreateTeam
    { createTeamName        :: !(Name Team)
    , createTeamDescription :: !(Maybe Text)
    , createTeamRepoNames   :: !(Vector (Name Repo))
    -- , createTeamPrivacy    :: Privacy
    , createTeamPermission  :: Permission
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeam where rnf = genericRnf
instance Binary CreateTeam

data EditTeam = EditTeam
    { editTeamName        :: !(Name Team)
    , editTeamDescription :: !(Maybe Text)
    -- , editTeamPrivacy :: Privacy
    , editTeamPermission  :: !Permission
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditTeam where rnf = genericRnf
instance Binary  EditTeam

data Role
    = RoleMaintainer
    | RoleMember
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Role
instance Binary Role

data ReqState
    = StatePending
    | StateActive
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ReqState where rnf = genericRnf
instance Binary ReqState

data TeamMembership = TeamMembership
    { teamMembershipUrl      :: !URL
    , teamMembershipRole     :: !Role
    , teamMembershipReqState :: !ReqState
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData TeamMembership where rnf = genericRnf
instance Binary TeamMembership

data CreateTeamMembership = CreateTeamMembership {
  createTeamMembershipRole :: !Role
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateTeamMembership where rnf = genericRnf
instance Binary CreateTeamMembership

-- JSON Instances

instance FromJSON SimpleTeam where
    parseJSON = withObject "SimpleTeam" $ \o -> SimpleTeam
        <$> o .: "id"
        <*> o .: "url"
        <*> o .: "name"
        <*> o .: "slug"
        <*> o .:?"description" .!= Nothing
        <*> o .:?"privacy" .!= Nothing
        <*> o .: "permission"
        <*> o .: "members_url"
        <*> o .: "repositories_url"

instance FromJSON Team where
    parseJSON = withObject "Team" $ \o -> Team
        <$> o .: "id"
        <*> o .: "url"
        <*> o .: "name"
        <*> o .: "slug"
        <*> o .:?"description" .!= Nothing
        <*> o .:?"privacy" .!= Nothing
        <*> o .: "permission"
        <*> o .: "members_url"
        <*> o .: "repositories_url"
        <*> o .: "members_count"
        <*> o .: "repos_count"
        <*> o .: "organization"

instance ToJSON CreateTeam where
  toJSON (CreateTeam name desc repo_names {-privacy-} permissions) =
    object [ "name"        .= name
           , "description" .= desc
           , "repo_names"  .= repo_names
           {-, "privacy" .= privacy-}
           , "permissions" .= permissions ]

instance ToJSON EditTeam where
  toJSON (EditTeam name desc {-privacy-} permissions) =
    object [ "name"        .= name
           , "description" .= desc
           {-, "privacy" .= privacy-}
           , "permissions" .= permissions ]

instance FromJSON TeamMembership where
    parseJSON = withObject "TeamMembership" $ \o -> TeamMembership
        <$> o .: "url"
        <*> o .: "role"
        <*> o .: "state"

instance FromJSON CreateTeamMembership where
    parseJSON = withObject "CreateTeamMembership" $ \o -> CreateTeamMembership
        <$> o .: "role"

instance ToJSON CreateTeamMembership where
    toJSON (CreateTeamMembership { createTeamMembershipRole = role }) =
        object [ "role" .= role ]

instance FromJSON AddTeamRepoPermission where
    parseJSON = withObject "AddTeamRepoPermission" $ \o -> AddTeamRepoPermission
        <$> o .: "permission"

instance ToJSON AddTeamRepoPermission where
    toJSON (AddTeamRepoPermission { addTeamRepoPermission = permission}) =
        object [ "permission" .= permission ]

instance FromJSON Role where
    parseJSON = withText "Attribute" $ \attr -> case attr of
        "maintainer" -> return RoleMaintainer
        "member"     -> return RoleMember
        _            -> fail $ "Unknown Role: " ++ show attr

instance ToJSON Role where
    toJSON RoleMaintainer = String "maintainer"
    toJSON RoleMember     = String "member"

instance ToJSON Permission where
    toJSON PermissionPull  = "pull"
    toJSON PermissionPush  = "push"
    toJSON PermissionAdmin = "admin"

instance FromJSON Permission where
    parseJSON = withText "Permission Attribute" $ \attr -> case attr of 
        "pull"  -> return PermissionPull
        "push"  -> return PermissionPush
        "admin" -> return PermissionAdmin
        _       -> fail $ "Unknown Permission Attribute: " ++ show attr

instance FromJSON Privacy where
    parseJSON  = withText "Privacy Attribute" $ \attr -> case attr of
        "secret" -> return PrivacySecret
        "closed" -> return PrivacyClosed
        _        -> fail $ "Unknown Privacy Attribute: " ++ show attr

instance ToJSON Privacy where
    toJSON PrivacySecret = String "secret"
    toJSON PrivacyClosed = String "closed"

instance FromJSON ReqState where
    parseJSON = withText "ReqState" $ \attr -> case attr of
        "active"  -> return StateActive
        "pending" -> return StatePending
        _         -> fail $ "Unknown ReqState: " ++ show attr

instance ToJSON ReqState where
    toJSON StateActive  = String "active"
    toJSON StatePending = String "pending"

-- | Filters members returned by their role in the team.
data TeamMemberRole
    = TeamMemberRoleAll         -- ^ all members of the team.
    | TeamMemberRoleMaintainer  -- ^ team maintainers
    | TeamMemberRoleMember      -- ^ normal members of the team.
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)
