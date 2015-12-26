{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Github.Request (
    GithubRequest(..),
    PostMethod(..),
    toMethod,
    Paths,
    QueryString,
    executeRequest,
    executeRequestWithMgr,
    executeRequest',
    executeRequestWithMgr',
    executeRequestMaybe,
    unsafeDropAuthRequirements,
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Data.Aeson.Compat    (FromJSON)
import Data.Typeable        (Typeable)
import GHC.Generics         (Generic)
import Network.HTTP.Conduit (Manager, httpLbs, newManager, tlsManagerSettings)

import qualified Data.ByteString.Lazy      as LBS
import qualified Network.HTTP.Types.Method as Method

import Github.Data    (Error)
import Github.Private (GithubAuth)

import qualified Github.Private as Private

------------------------------------------------------------------------------
-- Auxillary types
------------------------------------------------------------------------------

type Paths = [String]
type QueryString = String

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
    GithubGet       :: Paths -> QueryString -> GithubRequest k a
    GithubPost      :: PostMethod -> Paths -> LBS.ByteString -> GithubRequest 'True a
    GithubDelete    :: Paths -> GithubRequest 'True ()
    deriving (Typeable)

deriving instance Eq (GithubRequest k a)
deriving instance Ord (GithubRequest k a)

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
      where appPrec = 10 :: Int

------------------------------------------------------------------------------
-- Basic IO executor
------------------------------------------------------------------------------

-- | Execute 'GithubRequest' in 'IO'
executeRequest :: (FromJSON a, Show a)
               => GithubAuth -> GithubRequest k a -> IO (Either Error a)
executeRequest auth req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr manager auth req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequest' but with provided 'Manager'.
executeRequestWithMgr :: (FromJSON a, Show a)
                      => Manager
                      -> GithubAuth
                      -> GithubRequest k a
                      -> IO (Either Error a)
executeRequestWithMgr mgr auth req =
    case req of
        GithubGet paths qs ->
            Private.githubAPI' getResponse
                Method.methodGet
                (Private.buildPath paths ++ qs')
                (Just auth)
                Nothing
          where qs' | null qs   = ""
                    | otherwise = '?' : qs
        GithubPost m paths body ->
            Private.githubAPI' getResponse
                (toMethod m)
                (Private.buildPath paths)
                (Just auth)
                (Just body)
        GithubDelete paths ->
            Private.githubAPIDelete' getResponse
                auth
                (Private.buildPath paths)
  where
    getResponse = flip httpLbs mgr

-- | Like 'executeRequest' but without authentication.
executeRequest' :: (FromJSON a, Show a)
               => GithubRequest 'False a -> IO (Either Error a)
executeRequest' req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr' manager req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequestWithMgr' but without authentication.
executeRequestWithMgr' :: (FromJSON a, Show a)
                      => Manager
                      -> GithubRequest 'False a
                      -> IO (Either Error a)
executeRequestWithMgr' mgr req =
    case req of
        GithubGet paths qs ->
            Private.githubAPI' getResponse
                Method.methodGet
                (Private.buildPath paths ++ qs')
                Nothing
                Nothing
          where qs' | null qs   = ""
                    | otherwise = '?' : qs
  where
    getResponse = flip httpLbs mgr

-- | Helper for picking between 'executeRequest' and 'executeRequest''.
--
-- The use is discouraged.
executeRequestMaybe :: (FromJSON a, Show a)
                    => Maybe GithubAuth -> GithubRequest 'False a
                    -> IO (Either Error a)
executeRequestMaybe = maybe executeRequest' executeRequest

-- | Partial function to drop authentication need.
unsafeDropAuthRequirements :: GithubRequest 'True a -> GithubRequest k a
unsafeDropAuthRequirements (GithubGet ps qs) = GithubGet ps qs
unsafeDropAuthRequirements r                 =
    error $ "Trying to drop authenatication from" ++ show r
