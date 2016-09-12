-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.URL (
    URL(..),
    getUrl,
    ) where

import GitHub.Internal.Prelude
import Prelude ()

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
