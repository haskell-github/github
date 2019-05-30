-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Invitation where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T

data Invitation = Invitation
    { invitationId        :: !(Id Invitation)
    -- TODO: technically either one should be, maybe both. use `these` ?
    , invitationLogin     :: !(Maybe (Name User))
    , invitationEmail     :: !(Maybe Text)
    , invitationRole      :: !InvitationRole
    , invitationCreatedAt :: !UTCTime
    , inviter             :: !SimpleUser
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Invitation where rnf = genericRnf
instance Binary Invitation

instance FromJSON Invitation where
    parseJSON = withObject "Invitation" $ \o -> Invitation
        <$> o .: "id"
        <*> o .:? "login"
        <*> o .:? "email"
        <*> o .: "role"
        <*> o .: "created_at"
        <*> o .: "inviter"


data InvitationRole
    = InvitationRoleDirectMember
    | InvitationRoleAdmin
    | InvitationRoleBillingManager
    | InvitationRoleHiringManager
    | InvitationRoleReinstate
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData InvitationRole where rnf = genericRnf
instance Binary InvitationRole

instance FromJSON InvitationRole where
    parseJSON = withText "InvitationRole" $ \t -> case T.toLower t of
        "direct_member"   -> pure InvitationRoleDirectMember
        "admin"           -> pure InvitationRoleAdmin
        "billing_manager" -> pure InvitationRoleBillingManager
        "hiring_manager"  -> pure InvitationRoleHiringManager
        "reinstate"       -> pure InvitationRoleReinstate
        _                 -> fail $ "Unknown InvitationRole: " <> T.unpack t
