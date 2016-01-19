{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Data.Request (
    GithubRequest(..),
    CommandMethod(..),
    toMethod,
    StatusMap(..),
    MergeResult(..),
    Paths,
    IsPathPart(..),
    QueryString,
    Count,
    ) where

import Data.Aeson.Compat (FromJSON)
import Data.Hashable     (Hashable (..))
import Data.Typeable     (Typeable)
import Data.Vector       (Vector)
import GHC.Generics      (Generic)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Method as Method

import Github.Data.Id   (Id, untagId)
import Github.Data.Name (Name, untagName)

------------------------------------------------------------------------------
-- Auxillary types
------------------------------------------------------------------------------

type Paths = [String]
type QueryString = [(BS.ByteString, Maybe BS.ByteString)]
type Count = Int

class IsPathPart a where
    toPathPart :: a -> String

instance IsPathPart (Name a) where
    toPathPart = T.unpack . untagName

instance IsPathPart (Id a) where
    toPathPart = show . untagId

-- | Http method of requests with body.
data CommandMethod a where
    Post   :: CommandMethod a
    Patch  :: CommandMethod a
    Put    :: CommandMethod a
    Delete :: CommandMethod ()
    deriving (Typeable)

deriving instance Eq (CommandMethod a)

instance Show (CommandMethod a) where
    showsPrec _ Post    = showString "Post"
    showsPrec _ Patch   = showString "Patch"
    showsPrec _ Put     = showString "Put"
    showsPrec _ Delete  = showString "Delete"

instance Hashable (CommandMethod a) where
    hashWithSalt salt Post    = hashWithSalt salt (0 :: Int)
    hashWithSalt salt Patch   = hashWithSalt salt (1 :: Int)
    hashWithSalt salt Put     = hashWithSalt salt (2 :: Int)
    hashWithSalt salt Delete  = hashWithSalt salt (3 :: Int)

toMethod :: CommandMethod a -> Method.Method
toMethod Post   = Method.methodPost
toMethod Patch  = Method.methodPatch
toMethod Put    = Method.methodPut
toMethod Delete = Method.methodDelete

-- | Result of merge operation
data MergeResult = MergeSuccessful
                 | MergeCannotPerform
                 | MergeConflict
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)

instance Hashable MergeResult

-- | Status code transform
data StatusMap a where
    StatusOnlyOk :: StatusMap Bool
    StatusMerge  :: StatusMap MergeResult
    deriving (Typeable)

deriving instance Eq (StatusMap a)

instance Show (StatusMap a) where
    showsPrec _ StatusOnlyOk  = showString "StatusOnlyOK"
    showsPrec _ StatusMerge   = showString "StatusMerge"

instance Hashable (StatusMap a) where
    hashWithSalt salt StatusOnlyOk = hashWithSalt salt (0 :: Int)
    hashWithSalt salt StatusMerge  = hashWithSalt salt (1 :: Int)

------------------------------------------------------------------------------
-- Github request
------------------------------------------------------------------------------

-- | Github request data type.
--
-- * @k@ describes whether authentication is required. It's required for non-@GET@ requests.
-- * @a@ is the result type
--
-- /Note:/ 'GithubRequest' is not 'Functor' on purpose.
data GithubRequest (k :: Bool) a where
    GithubGet       :: FromJSON a => Paths -> QueryString -> GithubRequest k a
    GithubPagedGet  :: FromJSON (Vector a) => Paths -> QueryString -> Maybe Count -> GithubRequest k (Vector a)
    GithubCommand   :: FromJSON a => CommandMethod a -> Paths -> LBS.ByteString -> GithubRequest 'True a
    GithubStatus    :: StatusMap a -> GithubRequest k () -> GithubRequest k a
    deriving (Typeable)

deriving instance Eq (GithubRequest k a)

instance Show (GithubRequest k a) where
    showsPrec d r =
        case r of
            GithubGet ps qs -> showParen (d > appPrec) $
                showString "GithubGet "
                    . showsPrec (appPrec + 1) ps
                    . showString " "
                    . showsPrec (appPrec + 1) qs
            GithubPagedGet ps qs l -> showParen (d > appPrec) $
                showString "GithubPagedGet "
                    . showsPrec (appPrec + 1) ps
                    . showString " "
                    . showsPrec (appPrec + 1) qs
                    . showString " "
                    . showsPrec (appPrec + 1) l
            GithubCommand m ps body -> showParen (d > appPrec) $
                showString "GithubCommand "
                    . showsPrec (appPrec + 1) m
                    . showString " "
                    . showsPrec (appPrec + 1) ps
                    . showString " "
                    . showsPrec (appPrec + 1) body
            GithubStatus m req -> showParen (d > appPrec) $
                showString "GithubStatus "
                    . showsPrec (appPrec + 1) m
                    . showString " "
                    . showsPrec (appPrec + 1) req
      where appPrec = 10 :: Int

instance Hashable (GithubRequest k a) where
    hashWithSalt salt (GithubGet ps qs) =
        salt `hashWithSalt` (0 :: Int)
             `hashWithSalt` ps
             `hashWithSalt` qs
    hashWithSalt salt (GithubPagedGet ps qs l) =
        salt `hashWithSalt` (1 :: Int)
             `hashWithSalt` ps
             `hashWithSalt` qs
             `hashWithSalt` l
    hashWithSalt salt (GithubCommand m ps body) =
        salt `hashWithSalt` (2 :: Int)
             `hashWithSalt` m
             `hashWithSalt` ps
             `hashWithSalt` body
    hashWithSalt salt (GithubStatus sm req) =
        salt `hashWithSalt` (3 :: Int)
             `hashWithSalt` sm
             `hashWithSalt` req
