-- | The repo collaborators API as described on
-- <http://developer.github.com/v3/repos/collaborators/>.
module Github.Repos.Collaborators (
    collaboratorsOn,
    collaboratorsOn',
    collaboratorsOnR,
    isCollaboratorOn,
    isCollaboratorOnR,
    module Github.Data,
    ) where

import Data.Vector        (Vector)
import Github.Auth
import Github.Data
import Github.Request
import Network.HTTP.Types (Status)

-- | All the users who have collaborated on a repo.
--
-- > collaboratorsOn "thoughtbot" "paperclip"
collaboratorsOn :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector GithubOwner))
collaboratorsOn = collaboratorsOn' Nothing

-- | All the users who have collaborated on a repo.
-- With authentication.
collaboratorsOn' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector GithubOwner))
collaboratorsOn' auth user repo =
    executeRequestMaybe auth $ collaboratorsOnR user repo Nothing

-- | List collaborators.
-- See <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
collaboratorsOnR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector GithubOwner)
collaboratorsOnR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "collaborators"] []

-- | Whether the user is collaborating on a repo. Takes the user in question,
-- the user who owns the repo, and the repo name.
--
-- > isCollaboratorOn Nothing "mike-burns" "thoughtbot" "paperclip"
-- > isCollaboratorOn Nothing "johnson" "thoughtbot" "paperclip"
--
-- TODO: GithubStatus
isCollaboratorOn :: Maybe GithubAuth
                 -> Name GithubOwner  -- ^ Repository owner
                 -> Name Repo         -- ^ Repository name
                 -> Name GithubOwner  -- ^ Collaborator?
                 -> IO (Either Error Status)
isCollaboratorOn auth user repo coll =
    executeRequestMaybe auth $ isCollaboratorOnR user repo coll

-- | Check if a user is a collaborator.
-- See <https://developer.github.com/v3/repos/collaborators/#check-if-a-user-is-a-collaborator>
isCollaboratorOnR :: Name GithubOwner  -- ^ Repository owner
                  -> Name Repo         -- ^ Repository name
                  -> Name GithubOwner  -- ^ Collaborator?
                  -> GithubRequest k Status
isCollaboratorOnR user repo coll = GithubStatus $
    GithubGet ["repos", toPathPart user, toPathPart repo, "collaborators", toPathPart coll] []
