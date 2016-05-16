{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.URL (
    URL(..),
    getUrl,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson.Compat        (FromJSON (..), ToJSON (..), withText)
import Data.Binary.Orphans      (Binary)
import Data.Data                (Data, Typeable)
import Data.Text                (Text)
import GHC.Generics             (Generic)

-- | Data representing URLs in responses.
--
-- /N.B./ syntactical validity is not verified.
newtype URL = URL Text
    deriving (Eq, Ord, Show, Generic, Typeable, Data)

getUrl :: URL -> Text
getUrl (URL url) = url

instance NFData URL where rnf = genericRnf
instance Binary URL

instance ToJSON URL where
    toJSON (URL url) = toJSON url

instance FromJSON URL where
    parseJSON = withText "URL" (pure . URL)
