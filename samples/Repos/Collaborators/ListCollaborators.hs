module ListCollaborators where

import qualified Github.Repos.Collaborators as Github
import Data.List

main = do
  possibleCollaborators <- Github.collaboratorsOn "thoughtbot" "paperclip"
  case possibleCollaborators of
    (Left error) -> putStrLn $ "Error: " ++ (show error)
    (Right collaborators) ->
      putStrLn $ intercalate "\n" $ map formatAuthor collaborators

formatAuthor :: Github.GithubUser -> String
formatAuthor user =
  (Github.githubUserLogin user) ++ " (" ++ (Github.githubUserUrl user) ++ ")"
