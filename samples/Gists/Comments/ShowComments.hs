module ShowComments where

import qualified Github.Gists.Comments as Github
import Data.List (intercalate)

main = do
  possibleComments <- Github.commentsOn "1174060"
  case possibleComments of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right comments) -> putStrLn $ intercalate "\n\n" $ map formatComment comments

formatComment comment =
  (Github.githubOwnerLogin $ Github.gistCommentUser comment) ++ "\n" ++
    (formatDate $ Github.gistCommentUpdatedAt comment) ++ "\n\n" ++
    (Github.gistCommentBody comment)

formatDate = show . Github.fromDate
