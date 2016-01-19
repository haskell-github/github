{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Verification of incomming webhook payloads, as described at
-- <https://developer.github.com/webhooks/securing/>
module Github.Repos.Webhooks.Validate (
  isValidPayload
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import           Crypto.Hash
import           Data.Byteable          (constEqBytes, toBytes)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8  as BS
import           Data.Monoid


-- | Validates a given payload against a given HMAC hexdigest using a given
-- secret.
-- Returns 'True' iff the given hash is non-empty and it's a valid signature of
-- the payload.
isValidPayload
  :: String             -- ^ the secret
  -> Maybe String       -- ^ the hash provided by the remote party
                        -- in @X-Hub-Signature@ (if any),
                        -- including the 'sha1=...' prefix
  -> BS.ByteString      -- ^ the body
  -> Bool
isValidPayload secret shaOpt payload = maybe False (constEqBytes sign) shaOptBS
  where
    shaOptBS = BS.pack <$> shaOpt
    hexDigest = Hex.encode . toBytes . hmacGetDigest

    hm = hmac (BS.pack secret) payload :: HMAC SHA1
    sign = "sha1=" <> hexDigest hm
