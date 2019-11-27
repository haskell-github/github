-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github RateLimit API, as described at
-- <http://developer.github.com/v3/rate_limit/>.
module GitHub.Endpoints.RateLimit (
    rateLimitR,
    module GitHub.Data,
    ) where

import GitHub.Data
import Prelude ()

-- | Get your current rate limit status.
-- <https://developer.github.com/v3/rate_limit/#get-your-current-rate-limit-status>
rateLimitR :: Request k RateLimit
rateLimitR = query ["rate_limit"] []
