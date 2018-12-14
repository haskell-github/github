-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo invitations API as described on
-- <https://developer.github.com/v3/repos/invitations/>.
module GitHub.Endpoints.Repos.Invitations (
    listInvitationsOnR
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import GitHub.Data.Invitation


-- | List open invitations of a repository
-- See <https://developer.github.com/v3/repos/invitations/#list-invitations-for-a-repository>
listInvitationsOnR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector RepoInvitation)
listInvitationsOnR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "invitations"] []
