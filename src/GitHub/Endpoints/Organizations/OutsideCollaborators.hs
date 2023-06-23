-- |
-- The organization members API as described on
-- <https://developer.github.com/v3/orgs/outside_collaborators/>.

module GitHub.Endpoints.Organizations.OutsideCollaborators (
    outsideCollaboratorsR,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | All the users who are outside collaborators of the specified organization.
--
-- See <https://developer.github.com/v3/orgs/outside_collaborators/#list-outside-collaborators>
outsideCollaboratorsR :: Name Organization -> FetchCount -> Request k (Vector SimpleUser)
outsideCollaboratorsR organization =
    pagedQuery ["orgs", toPathPart organization, "outside_collaborators"] []
