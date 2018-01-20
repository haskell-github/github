{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
#define UNSAFE 1
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module also exports
-- @'FromJSON' a => 'FromJSON' ('HM.HashMap' 'Language' a)@
-- orphan-ish instance for @aeson < 1@
module GitHub.Data.Repos where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.Request     (IsPathPart (..))
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.HashMap.Strict as HM
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Types (FromJSONKey (..), fromJSONKeyCoerce)
#else
#ifdef UNSAFE
import Unsafe.Coerce (unsafeCoerce)
#endif
#endif

data Repo = Repo
    { repoSshUrl          :: !(Maybe URL)
    , repoDescription     :: !(Maybe Text)
    , repoCreatedAt       :: !(Maybe UTCTime)
    , repoHtmlUrl         :: !URL
    , repoSvnUrl          :: !(Maybe URL)
    , repoForks           :: !(Maybe Int)
    , repoHomepage        :: !(Maybe Text)
    , repoFork            :: !(Maybe Bool)
    , repoGitUrl          :: !(Maybe URL)
    , repoPrivate         :: !Bool
    , repoArchived        :: !Bool
    , repoCloneUrl        :: !(Maybe URL)
    , repoSize            :: !(Maybe Int)
    , repoUpdatedAt       :: !(Maybe UTCTime)
    , repoWatchers        :: !(Maybe Int)
    , repoOwner           :: !SimpleOwner
    , repoName            :: !(Name Repo)
    , repoLanguage        :: !(Maybe Language)
    , repoDefaultBranch   :: !(Maybe Text)
    , repoPushedAt        :: !(Maybe UTCTime)   -- ^ this is Nothing for new repositories
    , repoId              :: !(Id Repo)
    , repoUrl             :: !URL
    , repoOpenIssues      :: !(Maybe Int)
    , repoHasWiki         :: !(Maybe Bool)
    , repoHasIssues       :: !(Maybe Bool)
    , repoHasDownloads    :: !(Maybe Bool)
    , repoParent          :: !(Maybe RepoRef)
    , repoSource          :: !(Maybe RepoRef)
    , repoHooksUrl        :: !URL
    , repoStargazersCount :: !Int
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Repo where rnf = genericRnf
instance Binary Repo

data RepoRef = RepoRef
    { repoRefOwner :: !SimpleOwner
    , repoRefRepo  :: !(Name Repo)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RepoRef where rnf = genericRnf
instance Binary RepoRef

data NewRepo = NewRepo
    { newRepoName        :: !(Name Repo)
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

data EditRepo = EditRepo
    { editName         :: !(Maybe (Name Repo))
    , editDescription  :: !(Maybe Text)
    , editHomepage     :: !(Maybe Text)
    , editPublic       :: !(Maybe Bool)
    , editHasIssues    :: !(Maybe Bool)
    , editHasWiki      :: !(Maybe Bool)
    , editHasDownloads :: !(Maybe Bool)
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData EditRepo where rnf = genericRnf
instance Binary EditRepo

-- | Filter the list of the user's repos using any of these constructors.
data RepoPublicity
    = RepoPublicityAll     -- ^ All repos accessible to the user.
    | RepoPublicityOwner   -- ^ Only repos owned by the user.
    | RepoPublicityPublic  -- ^ Only public repos.
    | RepoPublicityPrivate -- ^ Only private repos.
    | RepoPublicityMember  -- ^ Only repos to which the user is a member but not an owner.
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

-- | The value is the number of bytes of code written in that language.
type Languages = HM.HashMap Language Int

-- | A programming language.
newtype Language = Language Text
   deriving (Show, Data, Typeable, Eq, Ord, Generic)

getLanguage :: Language -> Text
getLanguage (Language l) = l

instance NFData Language where rnf = genericRnf
instance Binary Language
instance Hashable Language where
    hashWithSalt salt (Language l) = hashWithSalt salt l
instance IsString Language where
    fromString = Language . fromString

data Contributor
    -- | An existing Github user, with their number of contributions, avatar
    -- URL, login, URL, ID, and Gravatar ID.
    = KnownContributor !Int !URL !(Name User) !URL !(Id User) !Text
    -- | An unknown Github user with their number of contributions and recorded name.
    | AnonymousContributor !Int !Text
   deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Contributor where rnf = genericRnf
instance Binary Contributor

contributorToSimpleUser :: Contributor -> Maybe SimpleUser
contributorToSimpleUser (AnonymousContributor _ _) = Nothing
contributorToSimpleUser (KnownContributor _contributions avatarUrl name url uid _gravatarid) =
    Just $ SimpleUser uid name avatarUrl url

-- JSON instances

instance FromJSON Repo where
    parseJSON = withObject "Repo" $ \o -> Repo <$> o .:? "ssh_url"
        <*> o .: "description"
        <*> o .:? "created_at"
        <*> o .: "html_url"
        <*> o .:? "svn_url"
        <*> o .:? "forks"
        <*> o .:? "homepage"
        <*> o .: "fork"
        <*> o .:? "git_url"
        <*> o .: "private"
        <*> o .:? "archived" .!= False
        <*> o .:? "clone_url"
        <*> o .:? "size"
        <*> o .:? "updated_at"
        <*> o .:? "watchers"
        <*> o .: "owner"
        <*> o .: "name"
        <*> o .:? "language"
        <*> o .:? "default_branch"
        <*> o .:? "pushed_at"
        <*> o .: "id"
        <*> o .: "url"
        <*> o .:? "open_issues"
        <*> o .:? "has_wiki"
        <*> o .:? "has_issues"
        <*> o .:? "has_downloads"
        <*> o .:? "parent"
        <*> o .:? "source"
        <*> o .: "hooks_url"
        <*> o .: "stargazers_count"

instance ToJSON NewRepo where
  toJSON (NewRepo { newRepoName         = name
                  , newRepoDescription  = description
                  , newRepoHomepage     = homepage
                  , newRepoPrivate      = private
                  , newRepoHasIssues    = hasIssues
                  , newRepoHasWiki      = hasWiki
                  , newRepoAutoInit     = autoInit
                  }) = object
                  [ "name"                .= name
                  , "description"         .= description
                  , "homepage"            .= homepage
                  , "private"             .= private
                  , "has_issues"          .= hasIssues
                  , "has_wiki"            .= hasWiki
                  , "auto_init"           .= autoInit
                  ]

instance ToJSON EditRepo where
  toJSON (EditRepo { editName         = name
                   , editDescription  = description
                   , editHomepage     = homepage
                   , editPublic       = public
                   , editHasIssues    = hasIssues
                   , editHasWiki      = hasWiki
                   , editHasDownloads = hasDownloads
                   }) = object
                   [ "name"          .= name
                   , "description"   .= description
                   , "homepage"      .= homepage
                   , "public"        .= public
                   , "has_issues"    .= hasIssues
                   , "has_wiki"      .= hasWiki
                   , "has_downloads" .= hasDownloads
                   ]

instance FromJSON RepoRef where
    parseJSON = withObject "RepoRef" $ \o -> RepoRef
        <$> o .: "owner"
        <*> o .: "name"

instance FromJSON Contributor where
    parseJSON = withObject "Contributor" $ \o -> do
        t <- o .: "type"
        case (t :: Text) of
            "Anonymous" -> AnonymousContributor
                <$> o .: "contributions"
                <*> o .: "name"
            _  -> KnownContributor
                <$> o .: "contributions"
                <*> o .: "avatar_url"
                <*> o .: "login"
                <*> o .: "url"
                <*> o .: "id"
                <*> o .: "gravatar_id"

instance FromJSON Language where
    parseJSON = withText "Language" (pure . Language)

instance ToJSON Language where
    toJSON = toJSON . getLanguage

#if MIN_VERSION_aeson(1,0,0)
instance FromJSONKey Language where
    fromJSONKey = fromJSONKeyCoerce
#else
instance FromJSON a => FromJSON (HM.HashMap Language a) where
    parseJSON = fmap mapKeyLanguage . parseJSON
      where
        mapKeyLanguage :: HM.HashMap Text a -> HM.HashMap Language a
#ifdef UNSAFE
        mapKeyLanguage = unsafeCoerce
#else
        mapKeyLanguage = mapKey Language
        mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> HM.HashMap k1 a -> HM.HashMap k2 a
        mapKey f = HM.fromList . map (first f) . HM.toList
#endif
#endif

data ArchiveFormat
    = ArchiveFormatTarball -- ^ ".tar.gz" format
    | ArchiveFormatZipball -- ^ ".zip" format
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

instance IsPathPart ArchiveFormat where
    toPathPart af = case af of
        ArchiveFormatTarball -> "tarball"
        ArchiveFormatZipball -> "zipball"
