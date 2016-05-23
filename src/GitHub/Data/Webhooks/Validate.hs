-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Verification of incomming webhook payloads, as described at
-- <https://developer.github.com/webhooks/securing/>
module GitHub.Data.Webhooks.Validate (
  isValidPayload
) where

import GitHub.Internal.Prelude

import Crypto.Hash     (HMAC, SHA1, hmac, hmacGetDigest)
import Data.Byteable   (constEqBytes, toBytes)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Base16 as Hex
import qualified Data.Text.Encoding     as TE

-- | Validates a given payload against a given HMAC hexdigest using a given
-- secret.
-- Returns 'True' iff the given hash is non-empty and it's a valid signature of
-- the payload.
isValidPayload
  :: Text           -- ^ the secret
  -> Maybe Text     -- ^ the hash provided by the remote party
                    -- in @X-Hub-Signature@ (if any),
                    -- including the 'sha1=...' prefix
  -> ByteString     -- ^ the body
  -> Bool
isValidPayload secret shaOpt payload = maybe False (constEqBytes sign) shaOptBS
  where
    shaOptBS = TE.encodeUtf8 <$> shaOpt
    hexDigest = Hex.encode . toBytes . hmacGetDigest

    hm = hmac (TE.encodeUtf8 secret) payload :: HMAC SHA1
    sign = "sha1=" <> hexDigest hm
