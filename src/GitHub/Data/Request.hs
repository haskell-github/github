{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Request (
    -- * Request
    Request,
    GenRequest (..),
    -- * Smart constructors
    query, pagedQuery, command,
    -- * Auxiliary types
    RW(..),
    CommandMethod(..),
    toMethod,
    FetchCount(..),
    MediaType (..),
    Paths,
    IsPathPart(..),
    QueryString,
    Count,
    ) where

import GitHub.Data.Definitions (Count, QueryString, IssueNumber, unIssueNumber)
import GitHub.Data.Id          (Id, untagId)
import GitHub.Data.Name        (Name, untagName)
import GitHub.Internal.Prelude

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Method as Method

------------------------------------------------------------------------------
-- Path parts
------------------------------------------------------------------------------

type Paths = [Text]

class IsPathPart a where
    toPathPart :: a -> Text

instance IsPathPart (Name a) where
    toPathPart = untagName

instance IsPathPart (Id a) where
    toPathPart = T.pack . show . untagId

instance IsPathPart IssueNumber where
    toPathPart = T.pack . show . unIssueNumber

-------------------------------------------------------------------------------
-- Command Method
-------------------------------------------------------------------------------

-- | Http method of requests with body.
data CommandMethod
    = Post
    | Patch
    | Put
    | Delete
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance Hashable CommandMethod

toMethod :: CommandMethod -> Method.Method
toMethod Post   = Method.methodPost
toMethod Patch  = Method.methodPatch
toMethod Put    = Method.methodPut
toMethod Delete = Method.methodDelete

-------------------------------------------------------------------------------
-- Fetch count
-------------------------------------------------------------------------------

-- | 'PagedQuery' returns just some results, using this data we can specify how
-- many pages we want to fetch.
data FetchCount = FetchAtLeast !Word | FetchAll
    deriving (Eq, Ord, Read, Show, Generic, Typeable)


-- | This instance is there mostly for 'fromInteger'.
instance Num FetchCount where
    fromInteger = FetchAtLeast . fromInteger

    FetchAtLeast a + FetchAtLeast b = FetchAtLeast (a * b)
    _ + _                           = FetchAll

    FetchAtLeast a * FetchAtLeast b = FetchAtLeast (a * b)
    _ * _                           = FetchAll

    abs    = error "abs @FetchCount: not implemented"
    signum = error "signum @FetchCount: not implemented"
    negate = error "negate @FetchCount: not implemented"

instance Hashable FetchCount
instance Binary FetchCount
instance NFData FetchCount where rnf = genericRnf

-------------------------------------------------------------------------------
-- MediaType
-------------------------------------------------------------------------------

data MediaType
    -- Standard media types
    = MtJSON     -- ^ @application/vnd.github.v3+json@
    | MtRaw      -- ^ @application/vnd.github.v3.raw@ <https://developer.github.com/v3/media/#raw-1>
    | MtDiff     -- ^ @application/vnd.github.v3.diff@ <https://developer.github.com/v3/media/#diff>
    | MtPatch    -- ^ @application/vnd.github.v3.patch@ <https://developer.github.com/v3/media/#patch>
    | MtSha      -- ^ @application/vnd.github.v3.sha@ <https://developer.github.com/v3/media/#sha>
    | MtStar     -- ^ @application/vnd.github.v3.star+json@ <https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1>
    | MtRedirect -- ^ <https://developer.github.com/v3/repos/contents/#get-archive-link>
    | MtStatus   -- ^ Parse status
    | MtUnit     -- ^ Always succeeds
    -- Media types for GitHub API previews
    | MtWyandottePreview       -- ^ @application/vnd.github.wyandotte-preview+json@ <https://developer.github.com/v3/previews/#migrations>
    | MtBarredRockPreview      -- ^ @application/vnd.github.barred-rock-preview+json@ <https://developer.github.com/v3/previews/#source-import>
    | MtAntManPreview          -- ^ @application/vnd.github.ant-man-preview+json@ <https://developer.github.com/v3/previews/#enhanced-deployments>
    | MtSquirrelGirlPreview    -- ^ @application/vnd.github.squirrel-girl-preview+json@ <https://developer.github.com/v3/previews/#reactions>
    | MtMockingbirdPreview     -- ^ @application/vnd.github.mockingbird-preview+json@ <https://developer.github.com/v3/previews/#timeline>
    | MtMisterFantasticPreview -- ^ @application/vnd.github.mister-fantastic-preview+json@ <https://developer.github.com/v3/previews/#pages>
    | MtMachineManPreview      -- ^ @application/vnd.github.machine-man-preview+json@ <https://developer.github.com/v3/previews/#integrations>
    | MtInertiaPreview         -- ^ @application/vnd.github.inertia-preview+json@ <https://developer.github.com/v3/previews/#projects>
    | MtCloakPreview           -- ^ @application/vnd.github.cloak-preview+json@ <https://developer.github.com/v3/previews/#commit-search>
    | MtBlackPantherPreview    -- ^ @application/vnd.github.black-panther-preview+json@ <https://developer.github.com/v3/previews/#community-profile-metrics>
    | MtGiantSentryFistPreview -- ^ @application/vnd.github.giant-sentry-fist-preview+json@ <https://developer.github.com/v3/previews/#user-blocking>
    | MtMercyPreview           -- ^ @application/vnd.github.mercy-preview+json@ <https://developer.github.com/v3/previews/#repository-topics>
    | MtScarletWitchPreview    -- ^ @application/vnd.github.scarlet-witch-preview+json@ <https://developer.github.com/v3/previews/#codes-of-conduct>
    | MtHellcatPreview         -- ^ @application/vnd.github.hellcat-preview+json@ <https://developer.github.com/v3/previews/#nested-teams>
    | MtNightshadePreview      -- ^ @application/vnd.github.nightshade-preview+json@ <https://developer.github.com/v3/previews/#repository-transfer>
    | MtSailorVPreview         -- ^ @application/vnd.github.sailor-v-preview+json@ <https://developer.github.com/v3/previews/#add-lock-reason>
    | MtDazzlerPreview         -- ^ @application/vnd.github.dazzler-preview+json@ <https://developer.github.com/v3/previews/#organization-invitations>
    | MtEchoPreview            -- ^ @application/vnd.github.echo-preview+json@ <https://developer.github.com/v3/previews/#team-discussions>
    | MtSymmetraPreview        -- ^ @application/vnd.github.symmetra-preview+json@ <https://developer.github.com/v3/previews/#label-emoji-search-and-descriptions>
    | MtZzzaxPreview           -- ^ @application/vnd.github.zzzax-preview+json@ <https://developer.github.com/v3/previews/#require-signed-commits>
    | MtLukeCagePreview        -- ^ @application/vnd.github.luke-cage-preview+json@ <https://developer.github.com/v3/previews/#require-multiple-approving-reviews>
    | MtHagarPreview           -- ^ @application/vnd.github.hagar-preview+json@ <https://developer.github.com/v3/previews/#retrieve-hovercard-information>
    | MtAntiopePreview         -- ^ @application/vnd.github.antiope-preview+json@ <https://developer.github.com/v3/previews/#check-runs-and-check-suites-api>
    | MtStarfoxPreview         -- ^ @application/vnd.github.starfox-preview+json@ <https://developer.github.com/v3/previews/#project-card-details>
    | MtFuryPreview            -- ^ @application/vnd.github.fury-preview+json@ <https://developer.github.com/v3/previews/#github-app-manifests>
    | MtFlashPreview           -- ^ @application/vnd.github.flash-preview+json@ <https://developer.github.com/v3/previews/#deployment-statuses>
    | MtSurturPreview          -- ^ @application/vnd.github.surtur-preview+json@ <https://developer.github.com/v3/previews/#repository-creation-permissions>
    | MtCorsairPreview         -- ^ @application/vnd.github.corsair-preview+json@ <https://developer.github.com/v3/previews/#content-attachments>
    | MtSombraPreview          -- ^ @application/vnd.github.sombra-preview+json@ <https://developer.github.com/v3/previews/#interaction-restrictions-for-repositories-and-organizations>
    | MtShadowCatPreview       -- ^ @application/vnd.github.shadow-cat-preview+json@ <https://developer.github.com/v3/previews/#draft-pull-requests>
    | MtSwitcherooPreview      -- ^ @application/vnd.github.switcheroo-preview+json@ <https://developer.github.com/v3/previews/#enable-and-disable-pages>
    | MtGrootPreview           -- ^ @application/vnd.github.groot-preview+json@ <https://developer.github.com/v3/previews/#list-branches-or-pull-requests-for-a-commit>
    | MtGambitPreview          -- ^ @application/vnd.github.gambit-preview+json@ <https://developer.github.com/v3/previews/#uninstall-a-github-app>
    | MtDorianPreview          -- ^ @application/vnd.github.dorian-preview+json@ <https://developer.github.com/v3/previews/#enable-or-disable-vulnerability-alerts-for-a-repository>
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

------------------------------------------------------------------------------
-- RW
------------------------------------------------------------------------------

-- | Type used as with @DataKinds@ to tag whether requests need authentication
-- or aren't read-only.
data RW
    = RO  -- ^ /Read-only/, doesn't necessarily requires authentication
    | RA  -- ^ /Read authenticated/
    | RW  -- ^ /Read-write/, requires authentication
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

{-
data SRO (rw :: RW) where
    ROO :: SRO 'RO
    ROA :: SRO 'RA

-- | This class is used to describe read-only (but pontentially
class    IReadOnly (rw :: RW) where iro :: SRO rw
instance IReadOnly 'RO        where iro = ROO
instance IReadOnly 'RA        where iro = ROA
-}

-------------------------------------------------------------------------------
-- GitHub Request
-------------------------------------------------------------------------------

-- | Github request data type.
--
-- * @rw@ describes whether authentication is required. It's required for non-@GET@ requests.
-- * @mt@ describes the media type, i.e. how the response should be interpreted.
-- * @a@ is the result type
--
-- /Note:/ 'Request' is not 'Functor' on purpose.
data GenRequest (mt :: MediaType) (rw :: RW) a where
    Query        :: Paths -> QueryString -> GenRequest mt rw a
    PagedQuery   :: Paths -> QueryString -> FetchCount -> GenRequest mt rw (Vector a)

    -- | Command
    Command
        :: CommandMethod           -- ^ command
        -> Paths                   -- ^ path
        -> LBS.ByteString          -- ^ body
        -> GenRequest mt 'RW a
  deriving (Typeable)

-- | Most requests ask for @JSON@.
type Request = GenRequest 'MtJSON

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

query :: Paths -> QueryString -> Request mt a
query ps qs = Query ps qs

pagedQuery :: FromJSON a => Paths -> QueryString -> FetchCount -> Request mt (Vector a)
pagedQuery ps qs fc = PagedQuery ps qs fc

command :: CommandMethod -> Paths -> LBS.ByteString -> Request 'RW a
command m ps body = Command m ps body

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

deriving instance Eq (GenRequest rw mt a)
deriving instance Ord (GenRequest rw mt a)
deriving instance Show (GenRequest rw mt a)

instance Hashable (GenRequest rw mt a) where
    hashWithSalt salt (Query ps qs) =
        salt `hashWithSalt` (0 :: Int)
             `hashWithSalt` ps
             `hashWithSalt` qs
    hashWithSalt salt (PagedQuery ps qs l) =
        salt `hashWithSalt` (1 :: Int)
             `hashWithSalt` ps
             `hashWithSalt` qs
             `hashWithSalt` l
    hashWithSalt salt (Command m ps body) =
        salt `hashWithSalt` (2 :: Int)
             `hashWithSalt` m
             `hashWithSalt` ps
             `hashWithSalt` body

-- TODO: Binary
