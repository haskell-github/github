-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Definitions where

import GitHub.Internal.Prelude
import Prelude ()

import Control.Monad       (mfilter)
import Data.Aeson.Types    (Parser)
import Network.HTTP.Client (HttpException)

import qualified Control.Exception as E
import qualified Data.ByteString   as BS
import qualified Data.Text         as T

import GitHub.Data.Id   (Id)
import GitHub.Data.Name (Name)
import GitHub.Data.URL  (URL (..))

-- | Errors have been tagged according to their source, so you can more easily
-- dispatch and handle them.
data Error
    = HTTPError !HttpException -- ^ A HTTP error occurred. The actual caught error is included.
    | ParseError !Text -- ^ An error in the parser itself.
    | JsonError !Text -- ^ The JSON is malformed or unexpected.
    | UserError !Text -- ^ Incorrect input.
    deriving (Show, Typeable)

instance E.Exception Error

-- | Type of the repository owners.
data OwnerType = OwnerUser | OwnerOrganization | OwnerBot
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable, Data)

instance NFData OwnerType
instance Binary OwnerType

data SimpleUser = SimpleUser
    { simpleUserId        :: !(Id User)
    , simpleUserLogin     :: !(Name User)
    , simpleUserAvatarUrl :: !URL
    , simpleUserUrl       :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleUser where rnf = genericRnf
instance Binary SimpleUser

data SimpleOrganization = SimpleOrganization
    { simpleOrganizationId        :: !(Id Organization)
    , simpleOrganizationLogin     :: !(Name Organization)
    , simpleOrganizationUrl       :: !URL
    , simpleOrganizationAvatarUrl :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization where rnf = genericRnf
instance Binary SimpleOrganization

-- | Sometimes we don't know the type of the owner, e.g. in 'Repo'
data SimpleOwner = SimpleOwner
    { simpleOwnerId        :: !(Id Owner)
    , simpleOwnerLogin     :: !(Name Owner)
    , simpleOwnerUrl       :: !URL
    , simpleOwnerAvatarUrl :: !URL
    , simpleOwnerType      :: !OwnerType
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOwner where rnf = genericRnf
instance Binary SimpleOwner

data User = User
    { userId          :: !(Id User)
    , userLogin       :: !(Name User)
    , userName        :: !(Maybe Text)
    , userType        :: !OwnerType  -- ^ Should always be 'OwnerUser' or 'OwnerBot'
    , userCreatedAt   :: !UTCTime
    , userPublicGists :: !Int
    , userAvatarUrl   :: !URL
    , userFollowers   :: !Int
    , userFollowing   :: !Int
    , userHireable    :: !(Maybe Bool)
    , userBlog        :: !(Maybe Text)
    , userBio         :: !(Maybe Text)
    , userPublicRepos :: !Int
    , userLocation    :: !(Maybe Text)
    , userCompany     :: !(Maybe Text)
    , userEmail       :: !(Maybe Text)
    , userUrl         :: !URL
    , userHtmlUrl     :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData User where rnf = genericRnf
instance Binary User

data Organization = Organization
    { organizationId          :: !(Id Organization)
    , organizationLogin       :: !(Name Organization)
    , organizationName        :: !(Maybe Text)
    , organizationType        :: !OwnerType  -- ^ Should always be 'OwnerOrganization'
    , organizationBlog        :: !(Maybe Text)
    , organizationLocation    :: !(Maybe Text)
    , organizationFollowers   :: !Int
    , organizationCompany     :: !(Maybe Text)
    , organizationAvatarUrl   :: !URL
    , organizationPublicGists :: !Int
    , organizationHtmlUrl     :: !URL
    , organizationEmail       :: !(Maybe Text)
    , organizationFollowing   :: !Int
    , organizationPublicRepos :: !Int
    , organizationUrl         :: !URL
    , organizationCreatedAt   :: !UTCTime
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Organization where rnf = genericRnf
instance Binary Organization

-- | In practice you can't have concrete values of 'Owner'.
newtype Owner = Owner (Either User Organization)
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Owner where rnf = genericRnf
instance Binary Owner

fromOwner :: Owner -> Either User Organization
fromOwner (Owner owner) = owner

-- JSON instances

instance FromJSON OwnerType where
    parseJSON = withText "OwnerType" $ \t -> case T.toLower t of
        "user"         -> pure $ OwnerUser
        "organization" -> pure $ OwnerOrganization
        "bot"          -> pure $ OwnerBot
        _              -> fail $ "Unknown OwnerType: " <> T.unpack t

instance FromJSON SimpleUser where
    parseJSON = withObject "SimpleUser" $ \obj -> do
        SimpleUser
            <$> obj .: "id"
            <*> obj .: "login"
            <*> obj .: "avatar_url"
            <*> obj .: "url"

instance FromJSON SimpleOrganization where
    parseJSON = withObject "SimpleOrganization" $ \obj ->
        SimpleOrganization
            <$> obj .: "id"
            <*> obj .: "login"
            <*> obj .: "url"
            <*> obj .: "avatar_url"

instance FromJSON SimpleOwner where
    parseJSON = withObject "SimpleOwner" $ \obj -> do
        SimpleOwner
            <$> obj .: "id"
            <*> obj .: "login"
            <*> obj .: "url"
            <*> obj .: "avatar_url"
            <*> obj .: "type"

parseUser :: Object -> Parser User
parseUser obj = User
    <$> obj .: "id"
    <*> obj .: "login"
    <*> obj .:? "name"
    <*> obj .: "type"
    <*> obj .: "created_at"
    <*> obj .: "public_gists"
    <*> obj .: "avatar_url"
    <*> obj .: "followers"
    <*> obj .: "following"
    <*> obj .:? "hireable"
    <*> obj .:? "blog"
    <*> obj .:? "bio"
    <*> obj .: "public_repos"
    <*> obj .:? "location"
    <*> obj .:? "company"
    <*> obj .:? "email"
    <*> obj .: "url"
    <*> obj .: "html_url"

parseOrganization :: Object -> Parser Organization
parseOrganization obj = Organization
    <$> obj .: "id"
    <*> obj .: "login"
    <*> obj .:? "name"
    <*> obj .: "type"
    <*> obj .:? "blog"
    <*> obj .:? "location"
    <*> obj .: "followers"
    <*> obj .:? "company"
    <*> obj .: "avatar_url"
    <*> obj .: "public_gists"
    <*> obj .: "html_url"
    <*> obj .:? "email"
    <*> obj .: "following"
    <*> obj .: "public_repos"
    <*> obj .: "url"
    <*> obj .: "created_at"

instance FromJSON User where
    parseJSON = mfilter ((/= OwnerOrganization) . userType) . withObject "User" parseUser

instance FromJSON Organization where
    parseJSON = withObject "Organization" parseOrganization

instance FromJSON Owner where
    parseJSON = withObject "Owner" $ \obj -> do
        t <- obj .: "type"
        case t of
            OwnerUser         -> Owner . Left <$> parseUser obj
            OwnerBot          -> Owner . Left <$> parseUser obj
            OwnerOrganization -> Owner . Right <$> parseOrganization obj

-- | Filter members returned in the list.
data OrgMemberFilter
    = OrgMemberFilter2faDisabled  -- ^ Members without two-factor authentication enabled. Available for organization owners.
    | OrgMemberFilterAll          -- ^ All members the authenticated user can see.
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

-- | Filter members returned by their role.
data OrgMemberRole
    = OrgMemberRoleAll     -- ^ All members of the organization, regardless of role.
    | OrgMemberRoleAdmin   -- ^ Organization owners.
    | OrgMemberRoleMember  -- ^ Non-owner organization members.
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

-- | Request query string
type QueryString = [(BS.ByteString, Maybe BS.ByteString)]

-- | Count of elements
type Count = Int



data MembershipRole
    = MembershipRoleMember
    | MembershipRoleAdmin
    | MembershipRoleBillingManager
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData MembershipRole where rnf = genericRnf
instance Binary MembershipRole

instance FromJSON MembershipRole where
    parseJSON = withText "MembershipRole" $ \t -> case T.toLower t of
        "member"   -> pure MembershipRoleMember
        "admin"           -> pure MembershipRoleAdmin
        "billing_manager" -> pure MembershipRoleBillingManager
        _                 -> fail $ "Unknown MembershipRole: " <> T.unpack t

data MembershipState
    = MembershipPending
    | MembershipActive
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData MembershipState where rnf = genericRnf
instance Binary MembershipState

instance FromJSON MembershipState where
    parseJSON = withText "MembershipState" $ \t -> case T.toLower t of
        "active"  -> pure MembershipActive
        "pending" -> pure MembershipPending
        _         -> fail $ "Unknown MembershipState: " <> T.unpack t


data Membership = Membership
    { membershipUrl             :: !URL
    , membershipState           :: !MembershipState
    , membershipRole            :: !MembershipRole
    , membershipOrganizationUrl :: !URL
    , membershipOrganization    :: !SimpleOrganization
    , membershipUser            :: !SimpleUser
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Membership where rnf = genericRnf
instance Binary Membership

instance FromJSON Membership where
    parseJSON = withObject "Membership" $ \o -> Membership
        <$> o .: "url"
        <*> o .: "state"
        <*> o .: "role"
        <*> o .: "organization_url"
        <*> o .: "organization"
        <*> o .: "user"


-------------------------------------------------------------------------------
-- IssueNumber
-------------------------------------------------------------------------------

newtype IssueNumber = IssueNumber Int
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

unIssueNumber :: IssueNumber -> Int
unIssueNumber (IssueNumber i) = i

instance Hashable IssueNumber
instance Binary IssueNumber

instance NFData IssueNumber where
    rnf (IssueNumber s) = rnf s

instance FromJSON IssueNumber where
    parseJSON = fmap IssueNumber . parseJSON

instance ToJSON IssueNumber where
    toJSON = toJSON . unIssueNumber

-------------------------------------------------------------------------------
-- IssueLabel
-------------------------------------------------------------------------------

data IssueLabel = IssueLabel
    { labelColor :: !Text
    , labelUrl   :: !URL
    , labelName  :: !(Name IssueLabel)
    , labelDesc  :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData IssueLabel where rnf = genericRnf
instance Binary IssueLabel

instance FromJSON IssueLabel where
    parseJSON = withObject "IssueLabel" $ \o -> IssueLabel
        <$> o .: "color"
        <*> o .:? "url" .!= URL "" -- in events there aren't URL
        <*> o .: "name"
        <*> o .:? "description"


-------------------------------------------------------------------------------
-- NewIssueLabel
-------------------------------------------------------------------------------

data NewIssueLabel = NewIssueLabel
    { newLabelColor :: !Text
    , newLabelName  :: !(Name NewIssueLabel)
    , newLabelDesc  :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewIssueLabel where rnf = genericRnf
instance Binary NewIssueLabel


instance ToJSON NewIssueLabel where
    toJSON (NewIssueLabel color lblName lblDesc) = object $ filter notNull
        [ "name" .= lblName
        , "color" .= color
        , "description" .= lblDesc
        ]
        where
            notNull (_, Null) = False
            notNull (_, _)    = True



-------------------------------------------------------------------------------
-- UpdateIssueLabel
-------------------------------------------------------------------------------

data UpdateIssueLabel = UpdateIssueLabel
    { updateLabelColor :: !Text
    , updateLabelName  :: !(Name UpdateIssueLabel)
    , updateLabelDesc  :: !(Maybe Text)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData UpdateIssueLabel where rnf = genericRnf
instance Binary UpdateIssueLabel


instance ToJSON UpdateIssueLabel where
    toJSON (UpdateIssueLabel color lblName lblDesc) = object $ filter notNull
        [ "new_name" .= lblName
        , "color" .= color
        , "description" .= lblDesc
        ]
        where
            notNull (_, Null) = False
            notNull (_, _)    = True
