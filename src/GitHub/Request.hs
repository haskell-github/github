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
-- This module provides data types and helper methods, which makes possible
-- to build alternative API request intepreters in addition to provided
-- 'IO' functions.
--
-- Simple example using @operational@ package. See @samples\/Operational\/Operational.hs@
--
-- > type GithubMonad a = Program (GH.Request 'False) a
-- >
-- > -- | Intepret GithubMonad value into IO
-- > runMonad :: Manager -> GH.Auth -> GithubMonad a -> ExceptT GH.Error IO a
-- > runMonad mgr auth m = case view m of
-- >    Return a   -> return a
-- >    req :>>= k -> do
-- >        b <- ExceptT $ GH.executeRequestWithMgr mgr auth req
-- >        runMonad mgr auth (k b)
-- >
-- > -- | Lift request into Monad
-- > githubRequest :: GH.Request 'False a -> GithubMonad a
-- > githubRequest = singleton
module GitHub.Request (
    -- * Types
    Request(..),
    CommandMethod(..),
    toMethod,
    Paths,
    QueryString,
    -- * Request execution in IO
    executeRequest,
    executeRequestWithMgr,
    executeRequest',
    executeRequestWithMgr',
    executeRequestMaybe,
    unsafeDropAuthRequirements,
    -- * Helpers
    makeHttpRequest,
    parseResponse,
    parseStatus,
    getNextUrl,
    performPagedRequest,
    ) where

import GitHub.Internal.Prelude
import Prelude ()

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError (..))
#else
import Control.Monad.Error (MonadError (..))
#endif

import Control.Monad.Catch        (MonadCatch (..), MonadThrow)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson.Compat          (eitherDecode)
import Data.List                  (find)

import Network.HTTP.Client
       (CookieJar, HttpException (..), Manager, RequestBody (..),
       Response (..), applyBasicAuth, checkStatus, httpLbs, method, newManager,
       requestBody, requestHeaders, setQueryString)
#if MIN_VERSION_http_client(0,4,30)
import Network.HTTP.Client (parseUrlThrow)
#else
import Network.HTTP.Client (parseUrl)
#endif
import Network.HTTP.Client.Internal (setUri)
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.HTTP.Link.Parser     (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link (..), LinkParam (..), href, linkParams)
import Network.HTTP.Types
       (Method, RequestHeaders, ResponseHeaders, Status (..))
import Network.URI                  (URI)

import qualified Control.Exception    as E
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Network.HTTP.Client  as HTTP

import GitHub.Auth         (Auth (..))
import GitHub.Data         (Error (..))
import GitHub.Data.Request

-- | Execute 'Request' in 'IO'
executeRequest :: Auth -> Request k a -> IO (Either Error a)
executeRequest auth req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr manager auth req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

lessFetchCount :: Int -> FetchCount -> Bool
lessFetchCount _ FetchAll         = True
lessFetchCount i (FetchAtLeast j) = i < fromIntegral j

-- | Like 'executeRequest' but with provided 'Manager'.
executeRequestWithMgr :: Manager
                      -> Auth
                      -> Request k a
                      -> IO (Either Error a)
executeRequestWithMgr mgr auth req = runExceptT $
    execute req
  where
    execute :: Request k a -> ExceptT Error IO a
    execute req' = case req' of
        Query {} -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs' httpReq
            parseResponse res
        PagedQuery _ _ l -> do
            httpReq <- makeHttpRequest (Just auth) req
            performPagedRequest httpLbs' predicate httpReq
          where
            predicate v = lessFetchCount (V.length v) l
        Command m _ _ -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs' httpReq
            case m of
                Delete -> pure ()
                _      -> parseResponse res
        StatusQuery sm _ -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs' httpReq
            parseStatus sm . responseStatus $ res
        HeaderQuery _ r -> do
            execute r
    httpLbs' :: HTTP.Request -> ExceptT Error IO (Response LBS.ByteString)
    httpLbs' req' = lift (httpLbs req' mgr) `catch` onHttpException

-- | Like 'executeRequest' but without authentication.
executeRequest' :: Request 'False a -> IO (Either Error a)
executeRequest' req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr' manager req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequestWithMgr' but without authentication.
executeRequestWithMgr' :: Manager
                       -> Request 'False a
                       -> IO (Either Error a)
executeRequestWithMgr' mgr req = runExceptT $
    execute req
  where
    execute :: Request 'False a -> ExceptT Error IO a
    execute req' = case req' of
        Query {} -> do
            httpReq <- makeHttpRequest Nothing req
            res <- httpLbs' httpReq
            parseResponse res
        PagedQuery _ _ l -> do
            httpReq <- makeHttpRequest Nothing req
            performPagedRequest httpLbs' predicate httpReq
          where
            predicate v = lessFetchCount (V.length v) l
        StatusQuery sm _ -> do
            httpReq <- makeHttpRequest Nothing req
            res <- httpLbs' httpReq
            parseStatus sm  . responseStatus $ res
        HeaderQuery _ r -> do
            execute r
    httpLbs' :: HTTP.Request -> ExceptT Error IO (Response LBS.ByteString)
    httpLbs' req' = lift (httpLbs req' mgr) `catch` onHttpException

-- | Helper for picking between 'executeRequest' and 'executeRequest''.
--
-- The use is discouraged.
executeRequestMaybe :: Maybe Auth -> Request 'False a
                    -> IO (Either Error a)
executeRequestMaybe = maybe executeRequest' executeRequest

-- | Partial function to drop authentication need.
unsafeDropAuthRequirements :: Request 'True a -> Request k a
unsafeDropAuthRequirements (Query ps qs) = Query ps qs
unsafeDropAuthRequirements r                 =
    error $ "Trying to drop authenatication from" ++ show r

------------------------------------------------------------------------------
-- Tools
------------------------------------------------------------------------------

-- | Create @http-client@ 'Request'.
--
-- * for 'PagedQuery', the initial request is created.
-- * for 'Status', the 'Request' for underlying 'Request' is created,
--   status checking is modifying accordingly.
--
-- @
-- parseResponse :: 'Maybe' 'Auth' -> 'Request' k a -> 'Maybe' 'Request'
-- @
makeHttpRequest :: MonadThrow m
                => Maybe Auth
                -> Request k a
                -> m HTTP.Request
makeHttpRequest auth r = case r of
    StatusQuery sm req -> do
        req' <- makeHttpRequest auth req
        return $ setCheckStatus (Just sm) req'
    Query paths qs -> do
        req <- parseUrl' $ url paths
        return $ setReqHeaders
               . setCheckStatus Nothing
               . setAuthRequest auth
               . setQueryString qs
               $ req
    PagedQuery paths qs _ -> do
        req <- parseUrl' $ url paths
        return $ setReqHeaders
               . setCheckStatus Nothing
               . setAuthRequest auth
               . setQueryString qs
               $ req
    Command m paths body -> do
        req <- parseUrl' $ url paths
        return $ setReqHeaders
               . setCheckStatus Nothing
               . setAuthRequest auth
               . setBody body
               . setMethod (toMethod m)
               $ req
    HeaderQuery h req -> do
        req' <- makeHttpRequest auth req
        return $ req' { requestHeaders = h <> requestHeaders req' }
  where
    parseUrl' :: MonadThrow m => Text -> m HTTP.Request
#if MIN_VERSION_http_client(0,4,30)
    parseUrl' = parseUrlThrow . T.unpack
#else
    parseUrl' = parseUrl . T.unpack
#endif

    url :: Paths -> Text
    url paths = baseUrl <> "/" <> T.intercalate "/" paths

    baseUrl :: Text
    baseUrl = case auth of
        Just (EnterpriseOAuth endpoint _)  -> endpoint
        _                                  -> "https://api.github.com"

    setReqHeaders :: HTTP.Request -> HTTP.Request
    setReqHeaders req = req { requestHeaders = reqHeaders <> requestHeaders req }

    setCheckStatus :: Maybe (StatusMap a) -> HTTP.Request -> HTTP.Request
    setCheckStatus sm req = req { checkStatus = successOrMissing sm }

    setMethod :: Method -> HTTP.Request -> HTTP.Request
    setMethod m req = req { method = m }

    reqHeaders :: RequestHeaders
    reqHeaders = maybe [] getOAuthHeader auth
        <> [("User-Agent", "github.hs/0.7.4")]
        <> [("Accept", "application/vnd.github.preview")]

    setBody :: LBS.ByteString -> HTTP.Request -> HTTP.Request
    setBody body req = req { requestBody = RequestBodyLBS body }

    setAuthRequest :: Maybe Auth -> HTTP.Request -> HTTP.Request
    setAuthRequest (Just (BasicAuth user pass)) = applyBasicAuth user pass
    setAuthRequest _                                  = id

    getOAuthHeader :: Auth -> RequestHeaders
    getOAuthHeader (OAuth token)             = [("Authorization", "token " <> token)]
    getOAuthHeader (EnterpriseOAuth _ token) = [("Authorization", "token " <> token)]
    getOAuthHeader _                         = []

    successOrMissing :: Maybe (StatusMap a) -> Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException
    successOrMissing sm s@(Status sci _) hs cookiejar
      | check     = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs cookiejar
      where
        check = case sm of
          Nothing            -> 200 <= sci && sci < 300
          Just StatusOnlyOk  -> sci == 204 || sci == 404
          Just StatusMerge   -> sci `elem` [204, 405, 409]

-- | Query @Link@ header with @rel=next@ from the request headers.
getNextUrl :: Response a -> Maybe URI
getNextUrl req = do
    linkHeader <- lookup "Link" (responseHeaders req)
    links <- parseLinkHeaderBS linkHeader
    nextURI <- find isRelNext links
    return $ href nextURI
  where
    isRelNext :: Link -> Bool
    isRelNext = any (== relNextLinkParam) . linkParams

    relNextLinkParam :: (LinkParam, Text)
    relNextLinkParam = (Rel, "next")

-- | Parse API response.
--
-- @
-- parseResponse :: 'FromJSON' a => 'Response' 'LBS.ByteString' -> 'Either' 'Error' a
-- @
parseResponse :: (FromJSON a, MonadError Error m) => Response LBS.ByteString -> m a
parseResponse res = case eitherDecode (responseBody res) of
    Right x  -> return x
    Left err -> throwError . ParseError . T.pack $ err

-- | Helper for handling of 'RequestStatus'.
--
-- @
-- parseStatus :: 'StatusMap' a -> 'Status' -> 'Either' 'Error' a
-- @
parseStatus :: MonadError Error m => StatusMap a -> Status -> m a
parseStatus StatusOnlyOk (Status sci _)
    | sci == 204 = return True
    | sci == 404 = return False
    | otherwise  = throwError $ JsonError $ "invalid status: " <> T.pack (show sci)
parseStatus StatusMerge (Status sci _)
    | sci == 204 = return MergeSuccessful
    | sci == 405 = return MergeCannotPerform
    | sci == 409 = return MergeConflict
    | otherwise  = throwError $ JsonError $ "invalid status: " <> T.pack (show sci)

-- | Helper for making paginated requests. Responses, @a@ are combined monoidally.
--
-- @
-- performPagedRequest :: ('FromJSON' a, 'Semigroup' a)
--                     => ('HTTP.Request' -> 'ExceptT' 'Error' 'IO' ('Response' 'LBS.ByteString'))
--                     -> (a -> 'Bool')
--                     -> 'HTTP.Request'
--                     -> 'ExceptT' 'Error' 'IO' a
-- @
performPagedRequest :: forall a m. (FromJSON a, Semigroup a, MonadCatch m, MonadError Error m)
                    => (HTTP.Request -> m (Response LBS.ByteString))  -- ^ `httpLbs` analogue
                    -> (a -> Bool)                                    -- ^ predicate to continue iteration
                    -> HTTP.Request                                   -- ^ initial request
                    -> m a
performPagedRequest httpLbs' predicate initReq = do
    res <- httpLbs' initReq
    m <- parseResponse res
    go m res initReq
  where
    go :: a -> Response LBS.ByteString -> HTTP.Request -> m a
    go acc res req =
        case (predicate acc, getNextUrl res) of
            (True, Just uri) -> do
                req' <- setUri req uri
                res' <- httpLbs' req'
                m <- parseResponse res'
                go (acc <> m) res' req'
            (_, _)           -> return acc

onHttpException :: MonadError Error m => HttpException -> m a
onHttpException = throwError . HTTPError
