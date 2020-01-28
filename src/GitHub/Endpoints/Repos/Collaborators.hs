-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo collaborators API as described on
-- <http://developer.github.com/v3/repos/collaborators/>.
module GitHub.Endpoints.Repos.Collaborators (
    collaboratorsOnR,
    collaboratorPermissionOnR,
    isCollaboratorOnR,
    addCollaboratorR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List collaborators.
-- See <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
collaboratorsOnR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector SimpleUser)
collaboratorsOnR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "collaborators"] []

-- | Review a user's permission level.
-- <https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level>
collaboratorPermissionOnR
    :: Name Owner        -- ^ Repository owner
    -> Name Repo         -- ^ Repository name
    -> Name User         -- ^ Collaborator to check permissions of.
    -> GenRequest 'MtJSON rw CollaboratorWithPermission
collaboratorPermissionOnR owner repo coll =
    query ["repos", toPathPart owner, toPathPart repo, "collaborators", toPathPart coll, "permission"] []

-- | Check if a user is a collaborator.
-- See <https://developer.github.com/v3/repos/collaborators/#check-if-a-user-is-a-collaborator>
isCollaboratorOnR
    :: Name Owner  -- ^ Repository owner
    -> Name Repo         -- ^ Repository name
    -> Name User         -- ^ Collaborator?
    -> GenRequest 'MtStatus rw Bool
isCollaboratorOnR user repo coll =
    Query ["repos", toPathPart user, toPathPart repo, "collaborators", toPathPart coll] []

-- | Invite a user as a collaborator.
-- See <https://developer.github.com/v3/repos/collaborators/#add-user-as-a-collaborator>
addCollaboratorR
    :: Name Owner        -- ^ Repository owner
    -> Name Repo         -- ^ Repository name
    -> Name User         -- ^ Collaborator to add
    -> GenRequest 'MtJSON 'RW (Maybe RepoInvitation)
addCollaboratorR owner repo coll =
    Command Put ["repos", toPathPart owner, toPathPart repo, "collaborators", toPathPart coll] mempty
