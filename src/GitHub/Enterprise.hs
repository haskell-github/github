-- |
-- This module re-exports all request constructors and data definitions for
-- working with GitHub Enterprise.

module GitHub.Enterprise (
    -- * Enterprise Admin
    -- | See <https://developer.github.com/enterprise/v3/enterprise-admin/>

    -- ** Organizations
    -- | See <https://developer.github.com/enterprise/v3/enterprise-admin/orgs/>
    createOrganizationR,
    renameOrganizationR,

    -- * Data definitions
    module GitHub.Data.Enterprise,
    ) where

import GitHub.Data.Enterprise
import GitHub.Endpoints.Enterprise.Organizations
