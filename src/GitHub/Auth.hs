-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Auth (
    Auth (..),
    AppAuth (..),
    AuthMethod,
    endpoint,
    setAuthRequest
    ) where

import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.ByteString     as BS
import qualified Network.HTTP.Client as HTTP

type Token = BS.ByteString

-- | The Github auth data type
data Auth
    = BasicAuth BS.ByteString BS.ByteString  -- ^ Username and password
    | OAuth Token                            -- ^ OAuth token
    | EnterpriseOAuth Text Token             -- ^ Custom endpoint and OAuth token
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Auth where rnf = genericRnf
instance Binary Auth
instance Hashable Auth

-- | The Github App auth data type
data AppAuth
    = JWT Token                 -- ^ JWT
    | EnterpriseJWT Text Token  -- ^ Custom endpoint and JWT
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData AppAuth where rnf = genericRnf
instance Binary AppAuth
instance Hashable AppAuth

-- | A type class for different authentication methods
class AuthMethod a where
    -- | Custom API endpoint without trailing slash
    endpoint       :: a -> Maybe Text
    -- | A function which sets authorisation on an HTTP request
    setAuthRequest :: a -> HTTP.Request -> HTTP.Request

instance AuthMethod Auth where
    endpoint (BasicAuth _ _)       = Nothing
    endpoint (OAuth _)             = Nothing
    endpoint (EnterpriseOAuth e _) = Just e

    setAuthRequest (BasicAuth u p)       = HTTP.applyBasicAuth u p
    setAuthRequest (OAuth t)             = setAuthHeader $ "token " <> t
    setAuthRequest (EnterpriseOAuth _ t) = setAuthHeader $ "token " <> t

instance AuthMethod AppAuth where
    endpoint (JWT _)             = Nothing
    endpoint (EnterpriseJWT e _) = Just e

    setAuthRequest (JWT t)             = setAuthHeader $ "Bearer " <> t
    setAuthRequest (EnterpriseJWT _ t) = setAuthHeader $ "Bearer " <> t

setAuthHeader :: BS.ByteString -> HTTP.Request -> HTTP.Request
setAuthHeader auth req =
    req { HTTP.requestHeaders = ("Authorization", auth) : HTTP.requestHeaders req }
