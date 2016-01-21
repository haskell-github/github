module ShowComment where

import qualified Github.Gists.Comments as Github

main = do
  possibleComment <- Github.comment "62449"
  case possibleComment of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right comment) -> putStrLn $ formatComment comment

formatComment comment =
  (Github.githubOwnerLogin $ Github.gistCommentUser comment) ++ "\n" ++
    (formatDate $ Github.gistCommentUpdatedAt comment) ++ "\n\n" ++
    (Github.gistCommentBody comment)

formatDate = show . Github.fromDate
