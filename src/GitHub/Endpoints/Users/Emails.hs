-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The user emails API as described on
-- <http://developer.github.com/v3/users/emails/>.
module GitHub.Endpoints.Users.Emails (
    currentUserEmails',
    currentUserEmailsR,
    currentUserPublicEmails',
    currentUserPublicEmailsR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | List email addresses for the authenticated user.
--
-- > currentUserEmails' (OAuth "token")
currentUserEmails' :: Auth -> IO (Either Error (Vector Email))
currentUserEmails' auth =
    executeRequest auth $ currentUserEmailsR FetchAll

-- | List email addresses.
-- See <https://developer.github.com/v3/users/emails/#list-email-addresses-for-a-user>
currentUserEmailsR :: FetchCount -> Request 'RA (Vector Email)
currentUserEmailsR =
    pagedQuery ["user", "emails"] []

-- | List public email addresses for the authenticated user.
--
-- > currentUserPublicEmails' (OAuth "token")
currentUserPublicEmails' :: Auth -> IO (Either Error (Vector Email))
currentUserPublicEmails' auth =
    executeRequest auth $ currentUserPublicEmailsR FetchAll

-- | List public email addresses.
-- See <https://developer.github.com/v3/users/emails/#list-public-email-addresses-for-a-user>
currentUserPublicEmailsR :: FetchCount -> Request 'RA (Vector Email)
currentUserPublicEmailsR =
    pagedQuery ["user", "public_emails"] []
