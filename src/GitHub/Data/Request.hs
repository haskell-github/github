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
    Request (..),
    SimpleRequest (..),
    -- * Smart constructors
    query, pagedQuery, command,
    -- * Auxiliary types
    RW(..),
    StatusMap,
    statusOnlyOk,
    CommandMethod(..),
    toMethod,
    FetchCount(..),
    Paths,
    IsPathPart(..),
    QueryString,
    Count,
    ) where

import GitHub.Data.Definitions (Count, QueryString)
import GitHub.Data.Id          (Id, untagId)
import GitHub.Data.Name        (Name, untagName)
import GitHub.Internal.Prelude

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as Types
import qualified Network.HTTP.Types.Method as Method
import Network.URI                         (URI)
------------------------------------------------------------------------------
-- Auxillary types
------------------------------------------------------------------------------

type Paths = [Text]

class IsPathPart a where
    toPathPart :: a -> Text

instance IsPathPart (Name a) where
    toPathPart = untagName

instance IsPathPart (Id a) where
    toPathPart = T.pack . show . untagId

-- | Http method of requests with body.
data CommandMethod a where
    Post   :: CommandMethod a
    Patch  :: CommandMethod a
    Put    :: CommandMethod a
    Put'   :: CommandMethod ()
    Delete :: CommandMethod ()
    deriving (Typeable)

deriving instance Eq (CommandMethod a)
deriving instance Ord (CommandMethod a)

instance Show (CommandMethod a) where
    showsPrec _ Post    = showString "Post"
    showsPrec _ Patch   = showString "Patch"
    showsPrec _ Put     = showString "Put"
    showsPrec _ Put'     = showString "Put'"
    showsPrec _ Delete  = showString "Delete"

instance Hashable (CommandMethod a) where
    hashWithSalt salt Post    = hashWithSalt salt (0 :: Int)
    hashWithSalt salt Patch   = hashWithSalt salt (1 :: Int)
    hashWithSalt salt Put     = hashWithSalt salt (2 :: Int)
    hashWithSalt salt Put'    = hashWithSalt salt (3 :: Int)
    hashWithSalt salt Delete  = hashWithSalt salt (4 :: Int)

toMethod :: CommandMethod a -> Method.Method
toMethod Post   = Method.methodPost
toMethod Patch  = Method.methodPatch
toMethod Put    = Method.methodPut
toMethod Put'   = Method.methodPut
toMethod Delete = Method.methodDelete

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

------------------------------------------------------------------------------
-- Github request
------------------------------------------------------------------------------

-- | Type used as with @DataKinds@ to tag whether requests need authentication
-- or aren't read-only.
data RW
    = RO  -- ^ /Read-only/, doesn't necessarily requires authentication
    | RA  -- ^ /Read autenticated/
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

-- | Github request data type.
--
-- * @k@ describes whether authentication is required. It's required for non-@GET@ requests.
-- * @a@ is the result type
--
-- /Note:/ 'Request' is not 'Functor' on purpose.
data Request (k :: RW) a where
    SimpleQuery   :: FromJSON a => SimpleRequest k a -> Request k a
    StatusQuery   :: StatusMap a -> SimpleRequest k () -> Request k a
    HeaderQuery   :: FromJSON a => Types.RequestHeaders -> SimpleRequest k a -> Request k a
    RedirectQuery :: SimpleRequest k () -> Request k URI
  deriving (Typeable)

data SimpleRequest (k :: RW) a where
    Query        :: Paths -> QueryString -> SimpleRequest k a
    PagedQuery   :: Paths -> QueryString -> FetchCount -> SimpleRequest k (Vector a)
    Command      :: CommandMethod a -> Paths -> LBS.ByteString -> SimpleRequest 'RW a
  deriving (Typeable)

-------------------------------------------------------------------------------
-- Status Map
-------------------------------------------------------------------------------

-- TODO: Change to 'Map' ?
type StatusMap a = [(Int, a)]

statusOnlyOk :: StatusMap Bool
statusOnlyOk =
    [ (204, True)
    , (404, False)
    ]

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

query :: FromJSON a => Paths -> QueryString -> Request k a
query ps qs = SimpleQuery (Query ps qs)

pagedQuery :: FromJSON a => Paths -> QueryString -> FetchCount -> Request k (Vector a)
pagedQuery ps qs fc = SimpleQuery (PagedQuery ps qs fc)

command :: FromJSON a => CommandMethod a -> Paths -> LBS.ByteString -> Request 'RW a
command m ps body = SimpleQuery (Command m ps body)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

deriving instance Eq a => Eq (Request k a)
deriving instance Eq a => Eq (SimpleRequest k a)

deriving instance Ord a => Ord (Request k a)
deriving instance Ord a => Ord (SimpleRequest k a)

instance Show (SimpleRequest k a) where
    showsPrec d r = showParen (d > appPrec) $ case r of
        Query ps qs -> showString "Query "
            . showsPrec (appPrec + 1) ps
            . showString " "
            . showsPrec (appPrec + 1) qs
        PagedQuery ps qs l -> showString "PagedQuery "
            . showsPrec (appPrec + 1) ps
            . showString " "
            . showsPrec (appPrec + 1) qs
            . showString " "
            . showsPrec (appPrec + 1) l
        Command m ps body -> showString "Command "
            . showsPrec (appPrec + 1) m
            . showString " "
            . showsPrec (appPrec + 1) ps
            . showString " "
            . showsPrec (appPrec + 1) body
      where
        appPrec = 10 :: Int

instance Show (Request k a) where
    showsPrec d r = showParen (d > appPrec) $ case r of
        SimpleQuery req -> showString "SimpleQuery "
            . showsPrec (appPrec + 1) req
        StatusQuery m req -> showString "Status "
            . showsPrec (appPrec + 1) (map fst m) -- !!! printing only keys
            . showString " "
            . showsPrec (appPrec + 1) req
        HeaderQuery m req -> showString "Header "
            . showsPrec (appPrec + 1) m
            . showString " "
            . showsPrec (appPrec + 1) req
        RedirectQuery req -> showString "Redirect "
            . showsPrec (appPrec + 1) req
      where
        appPrec = 10 :: Int

instance Hashable (SimpleRequest k a) where
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

instance Hashable (Request k a) where
    hashWithSalt salt (SimpleQuery req) =
        salt `hashWithSalt` (0 :: Int)
             `hashWithSalt` req
    hashWithSalt salt (StatusQuery sm req) =
        salt `hashWithSalt` (1 :: Int)
             `hashWithSalt` map fst sm
             `hashWithSalt` req
    hashWithSalt salt (HeaderQuery h req) =
        salt `hashWithSalt` (2 :: Int)
             `hashWithSalt` h
             `hashWithSalt` req
    hashWithSalt salt (RedirectQuery req) =
        salt `hashWithSalt` (3 :: Int)
             `hashWithSalt` req
