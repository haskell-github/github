{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    PageParams(..),
    PageLinks(..),
    MediaType (..),
    Paths,
    IsPathPart(..),
    QueryString,
    Count,
    ) where

import GitHub.Data.Definitions (Count, IssueNumber, QueryString, unIssueNumber)
import GitHub.Data.Id          (Id, untagId)
import GitHub.Data.Name        (Name, untagName)
import GitHub.Internal.Prelude

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Method as Method
import Network.URI (URI)

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
data FetchCount =
    FetchAtLeast !Word
    | FetchAll
    | FetchPage PageParams
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
-- PageParams
-------------------------------------------------------------------------------

-- | Params for specifying the precise page and items per page.
data PageParams = PageParams {
    pageParamsPerPage :: Maybe Int
    , pageParamsPage :: Maybe Int
    }
    deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance Hashable PageParams
instance Binary PageParams
instance NFData PageParams where rnf = genericRnf

-------------------------------------------------------------------------------
-- PageLinks
-------------------------------------------------------------------------------

-- | 'PagedQuery' returns just some results, using this data we can specify how
-- many pages we want to fetch.
data PageLinks = PageLinks {
    pageLinksPrev :: Maybe URI
    , pageLinksNext :: Maybe URI
    , pageLinksLast :: Maybe URI
    , pageLinksFirst :: Maybe URI
    }
    deriving (Eq, Ord, Show, Generic, Typeable)

instance NFData PageLinks where rnf = genericRnf

-------------------------------------------------------------------------------
-- MediaType
-------------------------------------------------------------------------------

data MediaType a
    = MtJSON       -- ^ @application/vnd.github.v3+json@
    | MtRaw        -- ^ @application/vnd.github.v3.raw@ <https://developer.github.com/v3/media/#raw-1>
    | MtDiff       -- ^ @application/vnd.github.v3.diff@ <https://developer.github.com/v3/media/#diff>
    | MtPatch      -- ^ @application/vnd.github.v3.patch@ <https://developer.github.com/v3/media/#patch>
    | MtSha        -- ^ @application/vnd.github.v3.sha@ <https://developer.github.com/v3/media/#sha>
    | MtStar       -- ^ @application/vnd.github.v3.star+json@ <https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1>
    | MtRedirect   -- ^ <https://developer.github.com/v3/repos/contents/#get-archive-link>
    | MtStatus     -- ^ Parse status
    | MtUnit       -- ^ Always succeeds
    | MtPreview  a -- ^ Some other (preview) type; this is an extension point.
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

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
data GenRequest (mt :: MediaType *) (rw :: RW) a where
    Query        :: Paths -> QueryString -> GenRequest mt rw a
    PagedQuery   :: (a ~ t b, Foldable t, Semigroup a) => Paths -> QueryString -> FetchCount -> GenRequest mt rw a

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
