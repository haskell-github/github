module Github.Repos.Collaborators (
 collaboratorsOn
,isCollaboratorOn
,module Github.Data
) where

import Github.Data
import Github.Private

import Data.ByteString.Char8 (pack)
import Network.HTTP.Enumerator (statusCode)

collaboratorsOn :: String -> String -> IO (Either Error [GithubUser])
collaboratorsOn userName repoName =
  githubGet ["repos", userName, repoName, "collaborators"]

isCollaboratorOn :: String -> String -> String -> IO (Either Error Bool)
isCollaboratorOn userName repoOwnerName repoName = do
  result <- doHttps (pack "GET")
                    (buildUrl ["repos", repoOwnerName, repoName, "collaborators", userName])
                    Nothing
  return $ either (Left . HTTPConnectionError)
                  (Right . (204 ==) . statusCode)
                  result
