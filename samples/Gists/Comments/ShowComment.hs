module ShowComment where

import qualified Github.Gists.Comments as Github
import Data.Default (def)

main = do
  possibleComment <- Github.comment def "62449"
  case possibleComment of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right comment) -> putStrLn $ formatComment comment

formatComment comment =
  (Github.githubOwnerLogin $ Github.gistCommentUser comment) ++ "\n" ++
    (formatGithubDate $ Github.gistCommentUpdatedAt comment) ++ "\n\n" ++
    (Github.gistCommentBody comment)

formatGithubDate = show . Github.fromGithubDate
