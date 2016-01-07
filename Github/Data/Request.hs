{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Github.Data.Request (
    GithubRequest(..),
    PostMethod(..),
    toMethod,
    Paths,
    QueryString,
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Data.Aeson.Compat    (FromJSON)
import Data.Typeable        (Typeable)
import GHC.Generics         (Generic)
import Network.HTTP.Types   (Status,)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Network.HTTP.Types.Method as Method

------------------------------------------------------------------------------
-- Auxillary types
------------------------------------------------------------------------------

type Paths = [String]
type QueryString = [(BS.ByteString, Maybe BS.ByteString)]

-- | Http method of requests with body.
data PostMethod = Post | Patch | Put
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)

toMethod :: PostMethod -> Method.Method
toMethod Post  = Method.methodPost
toMethod Patch = Method.methodPatch
toMethod Put   = Method.methodPut

------------------------------------------------------------------------------
-- Github request
------------------------------------------------------------------------------

-- | Github request data type.
--
-- * @k@ describes whether authentication is required. It's required for non-@GET@ requests.
-- * @a@ is the result type
--
-- /Note:/ 'GithubRequest' is not 'Functor' on purpose.
--
-- TODO: Add constructor for collection fetches.
data GithubRequest (k :: Bool) a where
    GithubGet       :: FromJSON a => Paths -> QueryString -> GithubRequest k a
    GithubPost      :: FromJSON a => PostMethod -> Paths -> LBS.ByteString -> GithubRequest 'True a
    GithubDelete    :: Paths -> GithubRequest 'True ()
    GithubStatus    :: GithubRequest k () -> GithubRequest k Status
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
            GithubPost m ps body -> showParen (d > appPrec) $
                showString "GithubPost "
                    . showsPrec (appPrec + 1) m
                    . showString " "
                    . showsPrec (appPrec + 1) ps
                    . showString " "
                    . showsPrec (appPrec + 1) body
            GithubDelete ps -> showParen (d > appPrec) $
                showString "GithubDelete "
                    . showsPrec (appPrec + 1) ps
            GithubStatus req -> showParen (d > appPrec) $
                showString "GithubStatus "
                    . showsPrec (appPrec + 1) req
      where appPrec = 10 :: Int
