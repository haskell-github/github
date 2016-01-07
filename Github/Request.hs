{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Github.Request (
    -- * Types
    GithubRequest(..),
    PostMethod(..),
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
    -- * Tools
    makeHttpRequest,
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Monad.Catch     (MonadThrow)
import Data.Aeson.Compat       (eitherDecode)
import Data.List               (intercalate)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (HttpException (..), Manager, Request (..),
                                RequestBody (..), Response (..), applyBasicAuth,
                                httpLbs, newManager, parseUrl, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types      (Method, RequestHeaders, Status (..),
                                methodDelete)

import qualified Control.Exception     as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T

import Github.Auth         (GithubAuth (..))
import Github.Data         (Error (..))
import Github.Data.Request

-- | Execute 'GithubRequest' in 'IO'
executeRequest :: Show a
               => GithubAuth -> GithubRequest k a -> IO (Either Error a)
executeRequest auth req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr manager auth req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequest' but with provided 'Manager'.
executeRequestWithMgr :: Show a
                      => Manager
                      -> GithubAuth
                      -> GithubRequest k a
                      -> IO (Either Error a)
executeRequestWithMgr mgr auth req =
    case req of
        GithubGet {} -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs httpReq mgr
            case eitherDecode (responseBody res) of
                Right x  -> pure . Right $ x
                Left err -> pure . Left . ParseError . T.pack $ err
        GithubPost {} -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs httpReq mgr
            case eitherDecode (responseBody res) of
                Right x  -> pure . Right $ x
                Left err -> pure . Left . ParseError . T.pack $ err
        GithubDelete {} -> do
            httpReq <- makeHttpRequest (Just auth) req
            _ <- httpLbs httpReq mgr
            pure . Right $ ()
        GithubStatus {} -> do
            httpReq <- makeHttpRequest (Just auth) req
            res <- httpLbs httpReq mgr
            pure . Right . responseStatus $ res

-- | Like 'executeRequest' but without authentication.
executeRequest' :: Show a
               => GithubRequest 'False a -> IO (Either Error a)
executeRequest' req = do
    manager <- newManager tlsManagerSettings
    x <- executeRequestWithMgr' manager req
#if !MIN_VERSION_http_client(0, 4, 18)
    closeManager manager
#endif
    pure x

-- | Like 'executeRequestWithMgr' but without authentication.
executeRequestWithMgr' :: Show a
                       => Manager
                       -> GithubRequest 'False a
                       -> IO (Either Error a)
executeRequestWithMgr' mgr req =
    case req of
        GithubGet {} -> do
            httpReq <- makeHttpRequest Nothing req
            res <- httpLbs httpReq mgr
            case eitherDecode (responseBody res) of
                Right x  -> pure . Right $ x
                Left err -> pure . Left . ParseError . T.pack $ err
        GithubStatus {} -> do
            httpReq <- makeHttpRequest Nothing req
            res <- httpLbs httpReq mgr
            pure . Right . responseStatus $ res

-- | Helper for picking between 'executeRequest' and 'executeRequest''.
--
-- The use is discouraged.
executeRequestMaybe :: Show a
                    => Maybe GithubAuth -> GithubRequest 'False a
                    -> IO (Either Error a)
executeRequestMaybe = maybe executeRequest' executeRequest

-- | Partial function to drop authentication need.
unsafeDropAuthRequirements :: GithubRequest 'True a -> GithubRequest k a
unsafeDropAuthRequirements (GithubGet ps qs) = GithubGet ps qs
unsafeDropAuthRequirements r                 =
    error $ "Trying to drop authenatication from" ++ show r

------------------------------------------------------------------------------
-- Tools
------------------------------------------------------------------------------

makeHttpRequest :: MonadThrow m
                => Maybe GithubAuth
                -> GithubRequest k a
                -> m Request
makeHttpRequest auth r = case r of
    GithubStatus req  -> makeHttpRequest auth req
    GithubGet paths qs -> do
        req <- parseUrl $ url paths
        pure $ setReqHeaders
             . setCheckStatus
             . setAuthRequest auth
             . setQueryString qs
             $ req
    GithubPost m paths body -> do
        req <- parseUrl $ url paths
        pure $ setReqHeaders
             . setCheckStatus
             . setAuthRequest auth
             . setBody body
             . setMethod (toMethod m)
             $ req
    GithubDelete paths -> do
        req <- parseUrl $ url paths
        pure $ setReqHeaders
             . setCheckStatus
             . setAuthRequest auth
             . setMethod methodDelete
             $ req
  where
    url :: Paths -> String
    url paths = baseUrl ++ '/' : intercalate "/" paths

    baseUrl :: String
    baseUrl = case auth of
        Just (GithubEnterpriseOAuth endpoint _)  -> endpoint
        _                                        -> "https://api.github.com"

    setReqHeaders :: Request -> Request
    setReqHeaders req = req { requestHeaders = reqHeaders <> requestHeaders req }

    setCheckStatus :: Request -> Request
    setCheckStatus req = req { checkStatus = successOrMissing }

    setMethod :: Method -> Request -> Request
    setMethod m req = req { method = m }

    reqHeaders :: RequestHeaders
    reqHeaders = maybe [] getOAuthHeader auth
        <> [("User-Agent", "github.hs/0.7.4")]
        <> [("Accept", "application/vnd.github.preview")]

    setBody :: LBS.ByteString -> Request -> Request
    setBody body req = req { requestBody = RequestBodyLBS body }

    setAuthRequest :: Maybe GithubAuth -> Request -> Request
    setAuthRequest (Just (GithubBasicAuth user pass)) = applyBasicAuth user pass
    setAuthRequest _                                  = id

    getOAuthHeader :: GithubAuth -> RequestHeaders
    getOAuthHeader (GithubOAuth token) = [("Authorization", BS8.pack ("token " ++ token))]
    getOAuthHeader _                   = []

    successOrMissing s@(Status sci _) hs cookiejar
      | (200 <= sci && sci < 300) || sci == 404 = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs cookiejar
