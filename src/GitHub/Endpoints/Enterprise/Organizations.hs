-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The GitHub Enterprise orgs API as described on <https://developer.github.com/enterprise/v3/enterprise-admin/orgs/>.
module GitHub.Endpoints.Enterprise.Organizations (
    createOrganizationR,
    renameOrganizationR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Create an organization.
-- See <https://developer.github.com/enterprise/v3/enterprise-admin/orgs/#create-an-organization>
createOrganizationR :: CreateOrganization -> Request 'RW SimpleOrganization
createOrganizationR =
    command Post ["admin", "organizations"] . encode

-- | Rename an organization.
-- See <https://developer.github.com/enterprise/v3/enterprise-admin/orgs/#rename-an-organization>
renameOrganizationR :: Name Organization -> RenameOrganization -> Request 'RW RenameOrganizationResponse
renameOrganizationR org =
    command Patch ["admin", "organizations", toPathPart org] . encode
