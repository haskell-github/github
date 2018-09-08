-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.RateLimit where

import GitHub.Internal.Prelude
import Prelude ()

data Limits = Limits
    { limitsMax       :: !Int
    , limitsRemaining :: !Int
    , limitsReset     :: !Int -- TODO: change to proper type
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Limits where rnf = genericRnf
instance Binary Limits

instance FromJSON Limits where
    parseJSON = withObject "Limits" $ \obj -> Limits
        <$> obj .: "limit"
        <*> obj .: "remaining"
        <*> obj .: "reset"

data RateLimit = RateLimit
    { rateLimitCore    :: Limits
    , rateLimitSearch  :: Limits
    , rateLimitGraphQL :: Limits
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RateLimit where rnf = genericRnf
instance Binary RateLimit

instance FromJSON RateLimit where
    parseJSON = withObject "RateLimit" $ \obj -> do
        resources <- obj .: "resources"
        RateLimit
            <$> resources .: "core"
            <*> resources .: "search"
            <*> resources .: "graphql"
