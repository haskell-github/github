{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
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
    -- * A convenient execution of requests
    github,
    github',
    GitHubRW,
    GitHubRO,
    -- * Types
    Request,
    GenRequest (..),
    CommandMethod(..),
    toMethod,
    Paths,
    QueryString,
    -- * Request execution in IO
    executeRequest,
    executeRequestWithMgr,
    executeRequestWithMgrAndRes,
    executeRequest',
    executeRequestWithMgr',
    executeRequestMaybe,
    unsafeDropAuthRequirements,
    -- * Helpers
    Accept (..),
    ParseResponse (..),
    makeHttpRequest,
    parseStatus,
    StatusMap,
    getNextUrl,
    performPagedRequest,
    parseResponseJSON,
    -- ** Preview
    PreviewAccept (..),
    PreviewParseResponse (..),
    -- * SSL
    -- | This always exist, independently of @openssl@ configuration flag.
    -- They change accordingly, to make use of the library simpler.
    withOpenSSL,
    tlsManagerSettings,


    -- preview types
    Inertia
    ) where

import GitHub.Internal.Prelude
import Prelude ()

import Control.Monad.Error.Class (MonadError (..))

import Control.Monad              (when)
import Control.Monad.Catch        (MonadCatch (..), MonadThrow)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson                 (eitherDecode)
import Data.List                  (find, intercalate)
import Data.String                (fromString)
import Data.Tagged                (Tagged (..))
import Data.Version               (showVersion)

import Network.HTTP.Client
       (HttpException (..), Manager, RequestBody (..), Response (..), getUri,
       httpLbs, method, newManager, redirectCount, requestBody, requestHeaders,
       setQueryString, setRequestIgnoreStatus)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types  (LinkParam (..), href, linkParams)
import Network.HTTP.Types       (Method, RequestHeaders, Status (..))
import Network.URI
       (URI, escapeURIString, isUnescapedInURIComponent, parseURIReference,
       relativeTo)

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.Internal as HTTP

#ifdef MIN_VERSION_http_client_tls
import Network.HTTP.Client.TLS (tlsManagerSettings)
#else
import Network.HTTP.Client.OpenSSL (opensslManagerSettings, withOpenSSL)

import qualified OpenSSL.Session          as SSL
import qualified OpenSSL.X509.SystemStore as SSL
#endif

import GitHub.Auth              (AuthMethod, endpoint, setAuthRequest)
import GitHub.Data              (Error (..))
import GitHub.Data.PullRequests (MergeResult (..))
import GitHub.Data.Request

import Paths_github (version)

-------------------------------------------------------------------------------
-- Convenience
-------------------------------------------------------------------------------

-- | A convenience function to turn functions returning @'Request' rw x@,
-- into functions returning @IO (Either 'Error' x)@.
--
-- >>> :t \auth -> github auth userInfoForR
-- \auth -> github auth userInfoForR
--   :: AuthMethod am => am -> Name User -> IO (Either Error User)
--
-- >>> :t github pullRequestsForR
-- \auth -> github auth pullRequestsForR
--   :: AuthMethod am =>
--      am
--      -> Name Owner
--      -> Name Repo
--      -> PullRequestMod
--      -> FetchCount
--      -> IO (Either Error (Data.Vector.Vector SimplePullRequest))
--
github :: (AuthMethod am, GitHubRW req res) => am -> req -> res
github = githubImpl

-- | Like 'github'' but for 'RO' i.e. read-only requests.
-- Note that GitHub has low request limit for non-authenticated requests.
--
-- >>> :t github' userInfoForR
-- github' userInfoForR :: Name User -> IO (Either Error User)
--
github' :: GitHubRO req res => req -> res
github' = githubImpl'

-- | A type-class implementing 'github'.
class GitHubRW req res | req -> res where
    githubImpl :: AuthMethod am => am -> req -> res

-- | A type-class implementing 'github''.
class GitHubRO req res | req -> res where
    githubImpl' :: req -> res

instance (ParseResponse mt req, res ~ Either Error req) => GitHubRW (GenRequest mt rw req) (IO res) where
    githubImpl = executeRequest

instance (ParseResponse mt req, res ~ Either Error req, rw ~ 'RO) => GitHubRO (GenRequest mt rw req) (IO res) where
    githubImpl' = executeRequest'

instance GitHubRW req res => GitHubRW (a -> req) (a -> res) where
    githubImpl am req x = githubImpl am (req x)

instance GitHubRO req res => GitHubRO (a -> req) (a -> res) where
    githubImpl' req x = githubImpl' (req x)

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_http_client_tls
withOpenSSL :: IO a -> IO a
withOpenSSL = id
#else
tlsManagerSettings :: HTTP.ManagerSettings
tlsManagerSettings = opensslManagerSettings $ do
    ctx <- SSL.context
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
    SSL.contextAddOption ctx SSL.SSL_OP_NO_TLSv1
    SSL.contextSetCiphers ctx "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256"
    SSL.contextLoadSystemCerts ctx
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
    return ctx
#endif

-- | Execute 'Request' in 'IO'
executeRequest
    :: (AuthMethod am, ParseResponse mt a)
    => am
    -> GenRequest mt rw a
    -> IO (Either Error a)
executeRequest auth req = withOpenSSL $ do
    manager <- newManager tlsManagerSettings
    executeRequestWithMgr manager auth req

lessFetchCount :: Int -> FetchCount -> Bool
lessFetchCount _ FetchAll         = True
lessFetchCount i (FetchAtLeast j) = i < fromIntegral j


-- | Like 'executeRequest' but with provided 'Manager'.
executeRequestWithMgr
    :: (AuthMethod am, ParseResponse mt a)
    => Manager
    -> am
    -> GenRequest mt rw a
    -> IO (Either Error a)
executeRequestWithMgr mgr auth req =
    fmap (fmap responseBody) (executeRequestWithMgrAndRes mgr auth req)

-- | Execute request and return the last received 'HTTP.Response'.
--
-- @since 0.24
executeRequestWithMgrAndRes
    :: (AuthMethod am, ParseResponse mt a)
    => Manager
    -> am
    -> GenRequest mt rw a
    -> IO (Either Error (HTTP.Response a))
executeRequestWithMgrAndRes mgr auth req = runExceptT $ do
    httpReq <- makeHttpRequest (Just auth) req
    performHttpReq httpReq req
  where
    httpLbs' :: HTTP.Request -> ExceptT Error IO (HTTP.Response LBS.ByteString)
    httpLbs' req' = lift (httpLbs req' mgr) `catch` onHttpException

    performHttpReq :: forall rw mt b. ParseResponse mt b => HTTP.Request -> GenRequest mt rw b -> ExceptT Error IO (HTTP.Response b)
    performHttpReq httpReq Query {} = do
        res <- httpLbs' httpReq
        (<$ res) <$> unTagged (parseResponse httpReq res :: Tagged mt (ExceptT Error IO b))

    performHttpReq httpReq (PagedQuery _ _ l) =
        unTagged (performPagedRequest httpLbs' predicate httpReq :: Tagged mt (ExceptT Error IO (HTTP.Response b)))
      where
        predicate v = lessFetchCount (length v) l

    performHttpReq httpReq (Command _ _ _) = do
        res <- httpLbs' httpReq
        (<$ res) <$> unTagged (parseResponse httpReq res :: Tagged mt (ExceptT Error IO b))

-- | Like 'executeRequest' but without authentication.
executeRequest' :: ParseResponse mt a => GenRequest mt 'RO a -> IO (Either Error a)
executeRequest' req = withOpenSSL $ do
    manager <- newManager tlsManagerSettings
    executeRequestWithMgr' manager req

-- | Like 'executeRequestWithMgr' but without authentication.
executeRequestWithMgr'
    :: ParseResponse mt a
    => Manager
    -> GenRequest mt 'RO a
    -> IO (Either Error a)
executeRequestWithMgr' mgr = executeRequestWithMgr mgr ()

-- | Helper for picking between 'executeRequest' and 'executeRequest''.
--
-- The use is discouraged.
executeRequestMaybe
    :: (AuthMethod am, ParseResponse mt a)
    => Maybe am
    -> GenRequest mt 'RO a
    -> IO (Either Error a)
executeRequestMaybe = maybe executeRequest' executeRequest

-- | Partial function to drop authentication need.
unsafeDropAuthRequirements :: GenRequest mt rw' a -> GenRequest mt rw a
unsafeDropAuthRequirements (Query ps qs) = Query ps qs
unsafeDropAuthRequirements r             =
    error $ "Trying to drop authenatication from" ++ show r

-------------------------------------------------------------------------------
-- Parse response
-------------------------------------------------------------------------------

class Accept (mt :: MediaType *) where
    contentType :: Tagged mt BS.ByteString
    contentType = Tagged "application/json" -- default is JSON

    modifyRequest :: Tagged mt (HTTP.Request -> HTTP.Request)
    modifyRequest = Tagged id

class Accept mt => ParseResponse (mt :: MediaType *) a where
    parseResponse
        :: MonadError Error m
        => HTTP.Request -> HTTP.Response LBS.ByteString
        -> Tagged mt (m a)

-------------------------------------------------------------------------------
-- JSON (+ star)
-------------------------------------------------------------------------------

-- | Parse API response.
--
-- @
-- parseResponse :: 'FromJSON' a => 'HTTP.Response' 'LBS.ByteString' -> 'Either' 'Error' a
-- @
parseResponseJSON :: (FromJSON a, MonadError Error m) => HTTP.Response LBS.ByteString -> m a
parseResponseJSON res = case eitherDecode (responseBody res) of
    Right x  -> return x
    Left err -> throwError . ParseError . T.pack $ err

instance Accept 'MtJSON where
    contentType = Tagged "application/vnd.github.v3+json"

instance FromJSON a => ParseResponse 'MtJSON a where
    parseResponse _ res = Tagged (parseResponseJSON res)

instance Accept 'MtStar where
    contentType = Tagged "application/vnd.github.v3.star+json"

instance FromJSON a => ParseResponse 'MtStar a where
    parseResponse _ res = Tagged (parseResponseJSON res)

-------------------------------------------------------------------------------
-- Raw / Diff / Patch / Sha
-------------------------------------------------------------------------------

instance Accept 'MtRaw   where contentType = Tagged "application/vnd.github.v3.raw"
instance Accept 'MtDiff  where contentType = Tagged "application/vnd.github.v3.diff"
instance Accept 'MtPatch where contentType = Tagged "application/vnd.github.v3.patch"
instance Accept 'MtSha   where contentType = Tagged "application/vnd.github.v3.sha"

instance a ~ LBS.ByteString => ParseResponse 'MtRaw   a where parseResponse _ = Tagged . return . responseBody
instance a ~ LBS.ByteString => ParseResponse 'MtDiff  a where parseResponse _ = Tagged . return . responseBody
instance a ~ LBS.ByteString => ParseResponse 'MtPatch a where parseResponse _ = Tagged . return . responseBody
instance a ~ LBS.ByteString => ParseResponse 'MtSha   a where parseResponse _ = Tagged . return . responseBody

-------------------------------------------------------------------------------
-- Redirect
-------------------------------------------------------------------------------

instance Accept 'MtRedirect where
    modifyRequest = Tagged $ \req ->
        setRequestIgnoreStatus $ req { redirectCount = 0 }

instance b ~ URI => ParseResponse 'MtRedirect b where
    parseResponse req = Tagged . parseRedirect (getUri req)

-- | Helper for handling of 'RequestRedirect'.
--
-- @
-- parseRedirect :: 'HTTP.Response' 'LBS.ByteString' -> 'Either' 'Error' a
-- @
parseRedirect :: MonadError Error m => URI -> HTTP.Response LBS.ByteString -> m URI
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

-------------------------------------------------------------------------------
-- Extension point
-------------------------------------------------------------------------------

class PreviewAccept p where
    previewContentType :: Tagged ('MtPreview p) BS.ByteString

    previewModifyRequest :: Tagged ('MtPreview p) (HTTP.Request -> HTTP.Request)
    previewModifyRequest = Tagged id

class PreviewAccept p => PreviewParseResponse p a where
    previewParseResponse
        :: MonadError Error m
        => HTTP.Request -> HTTP.Response LBS.ByteString
        -> Tagged ('MtPreview p) (m a)

instance PreviewAccept p => Accept ('MtPreview p) where
    contentType   = previewContentType
    modifyRequest = previewModifyRequest

instance PreviewParseResponse p a => ParseResponse ('MtPreview p) a where
    parseResponse = previewParseResponse


data Inertia

instance PreviewAccept Inertia where
  previewContentType = Tagged "application/vnd.github.inertia-preview+json"

instance FromJSON a => PreviewParseResponse Inertia a where
  previewParseResponse _ res = Tagged (parseResponseJSON res)


-------------------------------------------------------------------------------
-- Status
-------------------------------------------------------------------------------

instance Accept 'MtStatus where
    modifyRequest = Tagged setRequestIgnoreStatus

instance HasStatusMap a => ParseResponse 'MtStatus a where
    parseResponse _ = Tagged . parseStatus statusMap . responseStatus

type StatusMap a = [(Int, a)]

class HasStatusMap a where
    statusMap :: StatusMap a

instance HasStatusMap Bool where
    statusMap =
        [ (204, True)
        , (404, False)
        ]

instance HasStatusMap MergeResult where
    statusMap =
        [ (200, MergeSuccessful)
        , (405, MergeCannotPerform)
        , (409, MergeConflict)
        ]

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

-------------------------------------------------------------------------------
-- Unit
-------------------------------------------------------------------------------

-- | Note: we don't ignore response status.
--
-- We only accept any response body.
instance Accept 'MtUnit where

instance a ~ () => ParseResponse 'MtUnit a where
    parseResponse _ _ = Tagged (return ())

------------------------------------------------------------------------------
-- Tools
------------------------------------------------------------------------------

-- | Create @http-client@ 'Request'.
--
-- * for 'PagedQuery', the initial request is created.
-- * for 'Status', the 'Request' for underlying 'Request' is created,
--   status checking is modifying accordingly.
--
makeHttpRequest
    :: forall am mt rw a m. (AuthMethod am, MonadThrow m, Accept mt)
    => Maybe am
    -> GenRequest mt rw a
    -> m HTTP.Request
makeHttpRequest auth r = case r of
    Query paths qs -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . unTagged (modifyRequest :: Tagged mt (HTTP.Request -> HTTP.Request))
            . maybe id setAuthRequest auth
            . setQueryString qs
            $ req
    PagedQuery paths qs _ -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . unTagged (modifyRequest :: Tagged mt (HTTP.Request -> HTTP.Request))
            . maybe id setAuthRequest auth
            . setQueryString qs
            $ req
    Command m paths body -> do
        req <- parseUrl' $ url paths
        return
            $ setReqHeaders
            . unTagged (modifyRequest :: Tagged mt (HTTP.Request -> HTTP.Request))
            . maybe id setAuthRequest auth
            . setBody body
            . setMethod (toMethod m)
            $ req
  where
    parseUrl' :: MonadThrow m => String -> m HTTP.Request
    parseUrl' = HTTP.parseUrlThrow

    url :: Paths -> String
    url paths = maybe "https://api.github.com" T.unpack (endpoint =<< auth) ++ "/" ++ intercalate "/" paths' where
        paths' = map (escapeURIString isUnescapedInURIComponent . T.unpack) paths

    setReqHeaders :: HTTP.Request -> HTTP.Request
    setReqHeaders req = req { requestHeaders = reqHeaders <> requestHeaders req }

    setMethod :: Method -> HTTP.Request -> HTTP.Request
    setMethod m req = req { method = m }

    reqHeaders :: RequestHeaders
    reqHeaders = [("User-Agent", "github.hs/" <> fromString (showVersion version))] -- Version
        <> [("Accept", unTagged (contentType :: Tagged mt BS.ByteString))]

    setBody :: LBS.ByteString -> HTTP.Request -> HTTP.Request
    setBody body req = req { requestBody = RequestBodyLBS body }

-- | Query @Link@ header with @rel=next@ from the request headers.
getNextUrl :: HTTP.Response a -> Maybe URI
getNextUrl req = do
    linkHeader <- lookup "Link" (responseHeaders req)
    links <- parseLinkHeaderBS linkHeader
    nextURI <- find isRelNext links
    return $ href nextURI
  where
    -- isRelNext :: Link -> Bool or Link uri -> Bool
    isRelNext = any (== relNextLinkParam) . linkParams

    relNextLinkParam :: (LinkParam, Text)
    relNextLinkParam = (Rel, "next")

-- | Helper for making paginated requests. Responses, @a@ are combined monoidally.
--
-- The result is wrapped in the last received 'HTTP.Response'.
--
-- @
-- performPagedRequest :: ('FromJSON' a, 'Semigroup' a)
--                     => ('HTTP.Request' -> 'ExceptT' 'Error' 'IO' ('HTTP.Response' 'LBS.ByteString'))
--                     -> (a -> 'Bool')
--                     -> 'HTTP.Request'
--                     -> 'ExceptT' 'Error' 'IO' ('HTTP.Response' a)
-- @
performPagedRequest
    :: forall a m mt. (ParseResponse mt a, Semigroup a, MonadCatch m, MonadError Error m)
    => (HTTP.Request -> m (HTTP.Response LBS.ByteString))  -- ^ `httpLbs` analogue
    -> (a -> Bool)                                         -- ^ predicate to continue iteration
    -> HTTP.Request                                        -- ^ initial request
    -> Tagged mt (m (HTTP.Response a))
performPagedRequest httpLbs' predicate initReq = Tagged $ do
    res <- httpLbs' initReq
    m <- unTagged (parseResponse initReq res :: Tagged mt (m a))
    go m res initReq
  where
    go :: a -> HTTP.Response LBS.ByteString -> HTTP.Request -> m (HTTP.Response a)
    go acc res req =
        case (predicate acc, getNextUrl res) of
            (True, Just uri) -> do
                req' <- HTTP.setUri req uri
                res' <- httpLbs' req'
                m <- unTagged (parseResponse req' res' :: Tagged mt (m a))
                go (acc <> m) res' req'
            (_, _)           -> return (acc <$ res)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

onHttpException :: MonadError Error m => HttpException -> m a
onHttpException = throwError . HTTPError
