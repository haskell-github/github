-- | The repo collaborators API as described on
-- <http://developer.github.com/v3/repos/collaborators/>.
module Github.Repos.Collaborators (
 collaboratorsOn
,isCollaboratorOn
,module Github.Data
) where

import Github.Data
import Github.Private

import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Conduit as C (statusCode)
import qualified Network.HTTP.Types as T (statusCode)

-- | All the users who have collaborated on a repo.
--
-- > collaboratorsOn def "thoughtbot" "paperclip"
collaboratorsOn :: GithubConfig -> String -> String -> IO (Either Error [GithubOwner])
collaboratorsOn c userName repoName =
  githubGet c ["repos", userName, repoName, "collaborators"]

-- | Whether the user is collaborating on a repo. Takes the user in question,
-- the user who owns the repo, and the repo name.
--
-- > isCollaboratorOn def "mike-burns" "thoughtbot" "paperclip"
-- > isCollaboratorOn def "johnson" "thoughtbot" "paperclip"
isCollaboratorOn :: GithubConfig -> String -> String -> String -> IO (Either Error Bool)
isCollaboratorOn c userName repoOwnerName repoName = do
  result <- doHttps (getHttpManager c)
                    (pack "GET")
                    (buildUrl ["repos", repoOwnerName, repoName, "collaborators", userName])
                    Nothing
  return $ either (Left . HTTPConnectionError)
                  (Right . (204 ==) . T.statusCode . C.statusCode)
                  result
