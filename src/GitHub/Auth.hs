{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Auth where

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary              (Binary)
import Data.Data                (Data, Typeable)
import GHC.Generics             (Generic)

import qualified Data.ByteString as BS

-- | The Github auth data type
data Auth
    = BasicAuth BS.ByteString BS.ByteString
    | OAuth String -- ^ token
    | EnterpriseOAuth String  -- custom API endpoint without
                              -- trailing slash
                      String  -- token
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Auth where rnf = genericRnf
instance Binary Auth
