{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.RateLimit where

import GitHub.Internal.Prelude
import Prelude ()


data RateLimit = RateLimit
    { coreLimit       :: !Int
    , coreRemaining   :: !Int
    , coreReset       :: !Int
    , searchLimit     :: !Int
    , searchRemaining :: !Int
    , searchReset     :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RateLimit where rnf = genericRnf
instance Binary RateLimit

instance FromJSON RateLimit where
    parseJSON = withObject "RateLimit" $ \o -> do
        resources <- o .: "resources"
        core <- resources .: "core"
        coreLimit <- core .: "limit"
        coreRemaining <- core .: "remaining"
        coreReset <- core .: "reset"

        search <- resources .: "search"
        searchLimit <- search .: "limit"
        searchRemaining <- search .: "remaining"
        searchReset <- search .: "reset"

        return RateLimit{..}
