module ListCollaborators where

import qualified Github.Repos.Collaborators as Github
import Data.List
import Data.Default (def)

main = do
  possibleCollaborators <- Github.collaboratorsOn def "thoughtbot" "paperclip"
  case possibleCollaborators of
    (Left error) -> putStrLn $ "Error: " ++ (show error)
    (Right collaborators) ->
      putStrLn $ intercalate "\n" $ map formatAuthor collaborators

formatAuthor :: Github.GithubOwner -> String
formatAuthor user =
  (Github.githubOwnerLogin user) ++ " (" ++ (Github.githubOwnerUrl user) ++ ")"
