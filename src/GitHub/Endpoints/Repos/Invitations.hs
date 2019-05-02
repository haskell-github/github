-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo invitations API as described on
-- <https://developer.github.com/v3/repos/invitations/>.
module GitHub.Endpoints.Repos.Invitations (
    listInvitationsOnR,
    acceptInvitationFromR
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List open invitations of a repository
-- See <https://developer.github.com/v3/repos/invitations/#list-invitations-for-a-repository>
listInvitationsOnR :: Name Owner -> Name Repo -> FetchCount -> GenRequest 'MtJSON k (Vector RepoInvitation)
listInvitationsOnR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "invitations"] []

-- | Accept a repository invitation
-- See <https://developer.github.com/v3/repos/invitations/#accept-a-repository-invitation>
acceptInvitationFromR :: Id RepoInvitation -> GenRequest 'MtUnit 'RW ()
acceptInvitationFromR invId =
    Command Patch ["user", "repository_invitations", toPathPart invId] mempty
