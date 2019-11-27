-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
-- The public keys API, as described at
-- <https://developer.github.com/v3/users/keys/>
module GitHub.Endpoints.Users.PublicSSHKeys (
    -- * Querying public SSH keys
    publicSSHKeysR,
    publicSSHKeysForR,
    publicSSHKeyR,

    -- ** Create
    createUserPublicSSHKeyR,

    -- ** Delete
    deleteUserPublicSSHKeyR,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Querying public SSH keys.
-- See <https://developer.github.com/v3/users/keys/#list-public-keys-for-a-user>
publicSSHKeysForR :: Name Owner -> FetchCount -> Request 'RO (Vector PublicSSHKeyBasic)
publicSSHKeysForR user =
    pagedQuery ["users", toPathPart user, "keys"] []

-- | Querying the authenticated users' public SSH keys
-- See <https://developer.github.com/v3/users/keys/#list-your-public-keys>
publicSSHKeysR :: Request 'RA (Vector PublicSSHKey)
publicSSHKeysR =
    query ["user", "keys"] []

-- | Querying a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#get-a-single-public-key>
publicSSHKeyR :: Id PublicSSHKey -> Request 'RA PublicSSHKey
publicSSHKeyR keyId =
    query ["user", "keys", toPathPart keyId] []

-- | Create a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#create-a-public-key>.
createUserPublicSSHKeyR :: NewPublicSSHKey -> Request 'RW PublicSSHKey
createUserPublicSSHKeyR key =
    command Post ["user", "keys"] (encode key)

-- | Delete a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#delete-a-public-key>
deleteUserPublicSSHKeyR :: Id PublicSSHKey -> GenRequest 'MtUnit 'RW ()
deleteUserPublicSSHKeyR keyId =
    Command Delete ["user", "keys", toPathPart keyId] mempty
