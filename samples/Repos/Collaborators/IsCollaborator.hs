module IsCollaborator where

import qualified Github.Repos.Collaborators as Github
import Data.List

main = do
  let userName = "ubuwaits"
  possiblyIsCollaborator <- Github.isCollaboratorOn Nothing userName "thoughtbot" "paperclip"
  case possiblyIsCollaborator of
    (Left error) -> putStrLn $ "Error: " ++ (show error)
    (Right True) ->
      putStrLn $ userName ++ " is a collaborator on thoughtbot's paperclip"
    (Right False) ->
      putStrLn $ userName ++ " does not collaborate on thoughtbot's paperclip"
