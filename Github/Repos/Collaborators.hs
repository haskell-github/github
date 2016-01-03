-- | The repo collaborators API as described on
-- <http://developer.github.com/v3/repos/collaborators/>.
module Github.Repos.Collaborators (
 collaboratorsOn
,collaboratorsOn'
,isCollaboratorOn
,module Github.Data
) where

import Github.Data
import Github.Private

import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Conduit as C (responseStatus)
import qualified Network.HTTP.Types as T (statusCode)

-- | All the users who have collaborated on a repo.
--
-- > collaboratorsOn "thoughtbot" "paperclip"
collaboratorsOn :: String -> String -> IO (Either Error [GithubOwner])
collaboratorsOn userName reqRepoName =
  githubGet ["repos", userName, reqRepoName, "collaborators"]

-- | All the users who have collaborated on a repo.
-- With authentication.
collaboratorsOn' :: Maybe GithubAuth -> String -> String -> IO (Either Error [GithubOwner])
collaboratorsOn' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "collaborators"]

-- | Whether the user is collaborating on a repo. Takes the user in question,
-- the user who owns the repo, and the repo name.
--
-- > isCollaboratorOn Nothing "mike-burns" "thoughtbot" "paperclip"
-- > isCollaboratorOn Nothing "johnson" "thoughtbot" "paperclip"
isCollaboratorOn :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Bool)
isCollaboratorOn auth userName repoOwnerName reqRepoName = do
   result <- doHttps getResponseNewManager (pack "GET")
                     (apiEndpoint auth ++ buildPath ["repos", repoOwnerName, reqRepoName, "collaborators", userName])
                     Nothing
                     Nothing
   return $ either (Left . HTTPConnectionError)
                   (Right . (204 ==) . T.statusCode . C.responseStatus)
                   result
