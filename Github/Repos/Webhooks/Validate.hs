-- | Verification of incomming webhook payloads, as described at
-- <https://developer.github.com/webhooks/securing/>

module Github.Repos.Webhooks.Validate (
  isValidPayload
) where

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS


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
isValidPayload secret shaOpt payload = Just sign == shaOpt
  where
    hm = hmac (BS.pack secret) payload :: HMAC SHA1
    sign = "sha1=" ++ (show . hmacGetDigest $ hm)
