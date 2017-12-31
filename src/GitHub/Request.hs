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
    makeHttpSimpleRequest,
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

import Control.Monad              (when)
import Control.Monad.Catch        (MonadCatch (..), MonadThrow)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson.Compat          (eitherDecode)
import Data.List                  (find)

import Network.HTTP.Client
       (HttpException (..), Manager, RequestBody (..), Response (..),
       applyBasicAuth, getUri, httpLbs, method, newManager, redirectCount,
       requestBody, requestHeaders, setQueryString, setRequestIgnoreStatus)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types  (Link (..), LinkParam (..), href, linkParams)
import Network.HTTP.Types       (Method, RequestHeaders, Status (..))
import Network.URI              (URI, parseURIReference, relativeTo)

#if !MIN_VERSION_http_client(0,5,0)
import qualified Control.Exception  as E
import           Network.HTTP.Types (ResponseHeaders)
#endif

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Vector                  as V
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.Internal as HTTP

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
executeRequestWithMgr
    :: Manager
    -> Auth
    -> Request k a
    -> IO (Either Error a)
executeRequestWithMgr mgr auth req = runExceptT $ do
    httpReq <- makeHttpRequest (Just auth) req
    performHttpReq httpReq req
  where
    httpLbs' :: HTTP.Request -> ExceptT Error IO (Response LBS.ByteString)
    httpLbs' req' = lift (httpLbs req' mgr) `catch` onHttpException

    performHttpReq :: HTTP.Request -> Request k b -> ExceptT Error IO b
    performHttpReq httpReq (SimpleQuery sreq)   =
        performHttpReq' httpReq sreq
    performHttpReq httpReq (HeaderQuery _ sreq) =
        performHttpReq' httpReq sreq
    performHttpReq httpReq (StatusQuery sm _)   = do
        res <- httpLbs' httpReq
        parseStatus sm  . responseStatus $ res
    performHttpReq httpReq (RedirectQuery _)   = do
        res <- httpLbs' httpReq
        parseRedirect (getUri httpReq) res

    performHttpReq' :: FromJSON b => HTTP.Request -> SimpleRequest k b -> ExceptT Error IO b
    performHttpReq' httpReq Query {} = do
        res <- httpLbs' httpReq
        parseResponse res
    performHttpReq' httpReq (PagedQuery _ _ l) =
        performPagedRequest httpLbs' predicate httpReq
      where
        predicate v = lessFetchCount (V.length v) l
    performHttpReq' httpReq (Command m _ _) = do
        res <- httpLbs' httpReq
        case m of
             Delete -> pure ()
             Put'   -> pure ()
             _      -> parseResponse res


-- | Like 'executeRequest' but without authentication.
executeRequest' ::Request 'RO a -> IO (Either Error a)
executeRequest' req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr' manager req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequestWithMgr' but without authentication.
executeRequestWithMgr'
    :: Manager
    -> Request 'RO a
    -> IO (Either Error a)
executeRequestWithMgr' mgr req = runExceptT $ do
    httpReq <- makeHttpRequest Nothing req
    performHttpReq httpReq req
  where
    httpLbs' :: HTTP.Request -> ExceptT Error IO (Response LBS.ByteString)
    httpLbs' req' = lift (httpLbs req' mgr) `catch` onHttpException

    performHttpReq :: HTTP.Request -> Request 'RO b -> ExceptT Error IO b
    performHttpReq httpReq (SimpleQuery sreq)   =
        performHttpReq' httpReq sreq
    performHttpReq httpReq (HeaderQuery _ sreq) =
        performHttpReq' httpReq sreq
    performHttpReq httpReq (StatusQuery sm _)   = do
        res <- httpLbs' httpReq
        parseStatus sm  . responseStatus $ res
    performHttpReq httpReq (RedirectQuery _)   = do
        res <- httpLbs' httpReq
        parseRedirect (getUri httpReq) res

    performHttpReq' :: FromJSON b => HTTP.Request -> SimpleRequest 'RO b -> ExceptT Error IO b
    performHttpReq' httpReq Query {} = do
        res <- httpLbs' httpReq
        parseResponse res
    performHttpReq' httpReq (PagedQuery _ _ l) =
        performPagedRequest httpLbs' predicate httpReq
      where
        predicate v = lessFetchCount (V.length v) l

-- | Helper for picking between 'executeRequest' and 'executeRequest''.
--
-- The use is discouraged.
executeRequestMaybe :: Maybe Auth -> Request 'RO a -> IO (Either Error a)
executeRequestMaybe = maybe executeRequest' executeRequest

-- | Partial function to drop authentication need.
unsafeDropAuthRequirements :: Request k' a -> Request k a
unsafeDropAuthRequirements (SimpleQuery (Query ps qs)) =
    SimpleQuery (Query ps qs)
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
makeHttpRequest
    :: MonadThrow m
    => Maybe Auth
    -> Request k a
    -> m HTTP.Request
makeHttpRequest auth r = case r of
    SimpleQuery req ->
        makeHttpSimpleRequest auth req
    StatusQuery sm req -> do
        req' <- makeHttpSimpleRequest auth req
        return $ setCheckStatus (Just sm) req'
    HeaderQuery h req -> do
        req' <- makeHttpSimpleRequest auth req
        return $ req' { requestHeaders = h <> requestHeaders req' }
    RedirectQuery req -> do
        req' <- makeHttpSimpleRequest auth req
        return $ setRequestIgnoreStatus $ req' { redirectCount = 0 }

makeHttpSimpleRequest
    :: MonadThrow m
    => Maybe Auth
    -> SimpleRequest k a
    -> m HTTP.Request
makeHttpSimpleRequest auth r = case r of
    Query paths qs -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . setCheckStatus Nothing
            . setAuthRequest auth
            . setQueryString qs
            $ req
    PagedQuery paths qs _ -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . setCheckStatus Nothing
            . setAuthRequest auth
            . setQueryString qs
            $ req
    Command m paths body -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . setCheckStatus Nothing
            . setAuthRequest auth
            . setBody body
            . setMethod (toMethod m)
            $ req
  where
    parseUrl' :: MonadThrow m => Text -> m HTTP.Request
#if MIN_VERSION_http_client(0,4,30)
    parseUrl' = HTTP.parseRequest . T.unpack
#else
    parseUrl' = HTTP.parseUrl . T.unpack
#endif

    url :: Paths -> Text
    url paths = baseUrl <> "/" <> T.intercalate "/" paths

    baseUrl :: Text
    baseUrl = case auth of
        Just (EnterpriseOAuth endpoint _)  -> endpoint
        _                                  -> "https://api.github.com"

    setReqHeaders :: HTTP.Request -> HTTP.Request
    setReqHeaders req = req { requestHeaders = reqHeaders <> requestHeaders req }

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
parseStatus m (Status sci _) =
    maybe err return $ lookup sci m
  where
    err = throwError $ JsonError $ "invalid status: " <> T.pack (show sci)

-- | Helper for handling of 'RequestRedirect'.
--
-- @
-- parseRedirect :: 'Response' 'LBS.ByteString' -> 'Either' 'Error' a
-- @
parseRedirect :: MonadError Error m => URI -> Response LBS.ByteString -> m URI
parseRedirect originalUri rsp = do
    let status = responseStatus rsp
    when (statusCode status /= 302) $
        throwError $ ParseError $ "invalid status: " <> T.pack (show status)
    loc <- maybe noLocation return $ lookup "Location" $ responseHeaders rsp
    case parseURIReference $ T.unpack $ TE.decodeUtf8 loc of
        Nothing -> throwError $ ParseError $
            "location header does not contain a URI: " <> T.pack (show loc)
        Just uri -> return $ uri `relativeTo` originalUri
  where
    noLocation = throwError $ ParseError "no location header in response"

-- | Helper for making paginated requests. Responses, @a@ are combined monoidally.
--
-- @
-- performPagedRequest :: ('FromJSON' a, 'Semigroup' a)
--                     => ('HTTP.Request' -> 'ExceptT' 'Error' 'IO' ('Response' 'LBS.ByteString'))
--                     -> (a -> 'Bool')
--                     -> 'HTTP.Request'
--                     -> 'ExceptT' 'Error' 'IO' a
-- @
performPagedRequest
    :: forall a m. (FromJSON a, Semigroup a, MonadCatch m, MonadError Error m)
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
                req' <- HTTP.setUri req uri
                res' <- httpLbs' req'
                m <- parseResponse res'
                go (acc <> m) res' req'
            (_, _)           -> return acc

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------


setCheckStatus :: Maybe (StatusMap a) -> HTTP.Request -> HTTP.Request
#if MIN_VERSION_http_client(0,5,0)
setCheckStatus sm req = req { HTTP.checkResponse = successOrMissing sm }
#else
setCheckStatus sm req = req { HTTP.checkStatus = successOrMissing sm }
#endif


#if MIN_VERSION_http_client(0,5,0)
successOrMissing :: Maybe (StatusMap a) -> HTTP.Request -> HTTP.Response HTTP.BodyReader -> IO ()
successOrMissing sm _req res
    | check     = pure ()
    | otherwise = do
        chunk <- HTTP.brReadSome (HTTP.responseBody res) 1024
        let res' = fmap (const ()) res
        HTTP.throwHttp $ HTTP.StatusCodeException res' (LBS.toStrict chunk)
  where
    Status sci _ = HTTP.responseStatus res
#else
successOrMissing :: Maybe (StatusMap a) -> Status -> ResponseHeaders -> HTTP.CookieJar -> Maybe E.SomeException
successOrMissing sm s@(Status sci _) hs cookiejar
    | check     = Nothing
    | otherwise = Just $ E.toException $ StatusCodeException s hs cookiejar
  where
#endif
    check = case sm of
      Nothing  -> 200 <= sci && sci < 300
      Just sm' -> sci `elem` map fst sm'

onHttpException :: MonadError Error m => HttpException -> m a
onHttpException = throwError . HTTPError
