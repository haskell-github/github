-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Auth where

import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.ByteString as BS

type Token = BS.ByteString

-- | The Github auth data type
data Auth
    = BasicAuth BS.ByteString BS.ByteString
    | OAuth Token -- ^ token
    | EnterpriseOAuth Text    -- custom API endpoint without
                              -- trailing slash
                      Token   -- token
    | Bearer Token -- ^ bearer token
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Auth where rnf = genericRnf
instance Binary Auth
instance Hashable Auth
