-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github RateLimit API, as described at
-- <http://developer.github.com/v3/rate_limit/>.
module GitHub.Endpoints.RateLimit(
    rateLimit',
    rateLimit,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import qualified Data.Text.Encoding as TE

-- | Get your current rate limit status (Note: Accessing this endpoint does not count against your rate limit.)
-- With authentication.
rateLimit' :: Maybe Auth -> IO (Either Error RateLimit)
rateLimit' auth = executeRequestMaybe auth rateLimitR

-- | Get your current rate limit status (Note: Accessing this endpoint does not count against your rate limit.)
-- Without authentication.
--
-- > searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
rateLimit :: IO (Either Error RateLimit)
rateLimit = rateLimit' Nothing

rateLimitR :: Request k RateLimit
rateLimitR = query ["rate_limit"] []
