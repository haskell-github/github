{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Definitions where

import Prelude        ()
import Prelude.Compat

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad            (mfilter)
import Data.Aeson.Compat        (FromJSON (..), Object, withObject, withText,
                                 (.:), (.:?))
import Data.Aeson.Types         (Parser)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import GHC.Generics             (Generic)
import Network.HTTP.Client      (HttpException)

import qualified Control.Exception as E
import qualified Data.Text         as T

import Github.Data.Id
import Github.Data.Name

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
data OwnerType = OwnerUser | OwnerOrganization
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable, Data)

instance NFData OwnerType
instance Binary OwnerType

data SimpleUser = SimpleUser
    { simpleUserId        :: !(Id User)
    , simpleUserLogin     :: !(Name User)
    , simpleUserAvatarUrl :: !Text
    , simpleUserUrl       :: !Text
    , simpleUserType      :: !OwnerType  -- ^ Should always be 'OwnerUser'
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleUser where rnf = genericRnf
instance Binary SimpleUser

data SimpleOrganization = SimpleOrganization
    { simpleOrganizationId        :: !(Id Organization)
    , simpleOrganizationLogin     :: !(Name Organization)
    , simpleOrganizationUrl       :: !Text
    , simpleOrganizationAvatarUrl :: !Text
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOrganization where rnf = genericRnf
instance Binary SimpleOrganization

-- | Sometimes we don't know the type of the owner, e.g. in 'Repo'
data SimpleOwner = SimpleOwner
    { simpleOwnerId        :: !(Id GithubOwner)
    , simpleOwnerLogin     :: !(Name GithubOwner)
    , simpleOwnerUrl       :: !Text
    , simpleOwnerAvatarUrl :: !Text
    , simpleOwnerType      :: !OwnerType
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleOwner where rnf = genericRnf
instance Binary SimpleOwner

data User = User
    { userId          :: !(Id User)
    , userLogin       :: !(Name User)
    , userName        :: !(Maybe Text)
    , userType        :: !OwnerType  -- ^ Should always be 'OwnerUser'
    , userCreatedAt   :: !UTCTime
    , userPublicGists :: !Int
    , userAvatarUrl   :: !Text
    , userFollowers   :: !Int
    , userFollowing   :: !Int
    , userHireable    :: !(Maybe Bool)
    , userBlog        :: !(Maybe Text)
    , userBio         :: !(Maybe Text)
    , userPublicRepos :: !Int
    , userLocation    :: !(Maybe Text)
    , userCompany     :: !(Maybe Text)
    , userEmail       :: !(Maybe Text)
    , userUrl         :: !Text
    , userHtmlUrl     :: !Text
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
    , organizationAvatarUrl   :: !Text
    , organizationPublicGists :: !Int
    , organizationHtmlUrl     :: !Text
    , organizationEmail       :: !(Maybe Text)
    , organizationFollowing   :: !Int
    , organizationPublicRepos :: !Int
    , organizationUrl         :: !Text
    , organizationCreatedAt   :: !UTCTime
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Organization where rnf = genericRnf
instance Binary Organization

-- | In practic, you cam't have concrete values of 'GithubOwner'.
newtype GithubOwner = GithubOwner (Either User Organization)
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubOwner where rnf = genericRnf
instance Binary GithubOwner

fromGithubOwner :: GithubOwner -> Either User Organization
fromGithubOwner (GithubOwner owner) = owner

-- JSON instances

instance FromJSON OwnerType where
    parseJSON = withText "Owner type" $ \t ->
        case t of
            "User"          -> pure $ OwnerUser
            "Organization"  -> pure $ OwnerOrganization
            _               -> fail $ "Unknown owner type: " ++ T.unpack t

instance FromJSON SimpleUser where
    parseJSON = withObject "SimpleUser" $ \obj -> do
        SimpleUser
            <$> obj .: "id"
            <*> obj .: "login"
            <*> obj .: "avatar_url"
            <*> obj .: "url"
            <*> obj .: "type"

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
    parseJSON = mfilter ((== OwnerUser) . userType) . withObject "User" parseUser

instance FromJSON Organization where
    parseJSON = withObject "Organization" parseOrganization

instance FromJSON GithubOwner where
    parseJSON = withObject "GithubOwner" $ \obj -> do
        t <- obj .: "type"
        case t of
            OwnerUser         -> GithubOwner . Left <$> parseUser obj
            OwnerOrganization -> GithubOwner . Right <$> parseOrganization obj
