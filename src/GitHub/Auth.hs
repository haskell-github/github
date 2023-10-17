module GitHub.Auth (
    Auth (..),
    Token,
    JWTToken,
    AuthMethod,
    endpoint,
    setAuthRequest
    ) where

import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.ByteString     as BS
import qualified Data.Text.Encoding  as TE
import qualified Network.HTTP.Client as HTTP

type Token = BS.ByteString
type JWTToken = Text

-- | The Github auth data type
data Auth
    = BasicAuth BS.ByteString BS.ByteString  -- ^ Username and password
    | OAuth Token                            -- ^ OAuth token
    | JWT JWTToken                           -- ^ JWT Token
    | EnterpriseOAuth Text Token             -- ^ Custom endpoint and OAuth token
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Auth where rnf = genericRnf
instance Binary Auth
instance Hashable Auth

-- | A type class for different authentication methods
--
-- Note the '()' intance, which doee nothing, i.e. is unauthenticated.
class AuthMethod a where
    -- | Custom API endpoint without trailing slash
    endpoint       :: a -> Maybe Text
    -- | A function which sets authorisation on an HTTP request
    setAuthRequest :: a -> HTTP.Request -> HTTP.Request

instance AuthMethod () where
    endpoint _ = Nothing
    setAuthRequest _ = id

instance AuthMethod Auth where
    endpoint (BasicAuth _ _)       = Nothing
    endpoint (OAuth _)             = Nothing
    endpoint (JWT _)             = Nothing
    endpoint (EnterpriseOAuth e _) = Just e

    setAuthRequest (BasicAuth u p)       = HTTP.applyBasicAuth u p
    setAuthRequest (OAuth t)             = setAuthHeader $ "token " <> t
    setAuthRequest (JWT t)               = setAuthHeader $ "Bearer " <> TE.encodeUtf8 t
    setAuthRequest (EnterpriseOAuth _ t) = setAuthHeader $ "token " <> t

setAuthHeader :: BS.ByteString -> HTTP.Request -> HTTP.Request
setAuthHeader auth req =
    req { HTTP.requestHeaders = ("Authorization", auth) : HTTP.requestHeaders req }
