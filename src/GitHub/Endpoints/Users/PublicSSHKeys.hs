-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Todd Mohney <toddmohney@gmail.com>
--
-- The public keys API, as described at
-- <https://developer.github.com/v3/users/keys/>
module GitHub.Endpoints.Users.PublicSSHKeys (
    -- * Querying public SSH keys
    publicSSHKeys',
    publicSSHKeysR,
    publicSSHKeysFor',
    publicSSHKeysForR,
    publicSSHKey',
    publicSSHKeyR,

    -- ** Create
    createUserPublicSSHKey',
    createUserPublicSSHKeyR,

    -- ** Delete
    deleteUserPublicSSHKey',
    deleteUserPublicSSHKeyR,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | Querying public SSH keys.
publicSSHKeysFor' :: Name Owner -> IO (Either Error (Vector PublicSSHKeyBasic))
publicSSHKeysFor' user =
    executeRequest' $ publicSSHKeysForR user FetchAll

-- | Querying public SSH keys.
-- See <https://developer.github.com/v3/users/keys/#list-public-keys-for-a-user>
publicSSHKeysForR :: Name Owner -> FetchCount -> Request 'RO (Vector PublicSSHKeyBasic)
publicSSHKeysForR user =
    pagedQuery ["users", toPathPart user, "keys"] []

-- | Querying the authenticated users' public SSH keys
publicSSHKeys' :: Auth -> IO (Either Error (Vector PublicSSHKey))
publicSSHKeys' auth =
    executeRequest auth publicSSHKeysR

-- | Querying the authenticated users' public SSH keys
-- See <https://developer.github.com/v3/users/keys/#list-your-public-keys>
publicSSHKeysR :: Request 'RA (Vector PublicSSHKey)
publicSSHKeysR =
    query ["user", "keys"] []

-- | Querying a public SSH key
publicSSHKey' :: Auth -> Id PublicSSHKey -> IO (Either Error PublicSSHKey)
publicSSHKey' auth keyId =
    executeRequest auth $ publicSSHKeyR keyId

-- | Querying a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#get-a-single-public-key>
publicSSHKeyR :: Id PublicSSHKey -> Request 'RA PublicSSHKey
publicSSHKeyR keyId =
    query ["user", "keys", toPathPart keyId] []

-- | Create a public SSH key
createUserPublicSSHKey' :: Auth -> NewPublicSSHKey -> IO (Either Error PublicSSHKey)
createUserPublicSSHKey' auth key =
    executeRequest auth $ createUserPublicSSHKeyR key

-- | Create a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#create-a-public-key>.
createUserPublicSSHKeyR :: NewPublicSSHKey -> Request 'RW PublicSSHKey
createUserPublicSSHKeyR key =
    command Post ["user", "keys"] (encode key)

deleteUserPublicSSHKey' :: Auth -> Id PublicSSHKey -> IO (Either Error ())
deleteUserPublicSSHKey' auth keyId =
    executeRequest auth $ deleteUserPublicSSHKeyR keyId

-- | Delete a public SSH key.
-- See <https://developer.github.com/v3/users/keys/#delete-a-public-key>
deleteUserPublicSSHKeyR :: Id PublicSSHKey -> GenRequest 'MtUnit 'RW ()
deleteUserPublicSSHKeyR keyId =
    Command Delete ["user", "keys", toPathPart keyId] mempty
