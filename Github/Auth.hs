{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Github.Auth where

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import GHC.Generics             (Generic)

import qualified Data.ByteString as BS

-- | The Github auth data type
data GithubAuth = GithubBasicAuth BS.ByteString BS.ByteString
                | GithubOAuth String -- ^ token
                | GithubEnterpriseOAuth String  -- custom API endpoint without
                                                -- trailing slash
                                        String  -- token
                deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData GithubAuth where rnf = genericRnf
instance Binary GithubAuth
