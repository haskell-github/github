{-# LANGUAGE OverloadedStrings #-}

module CreateIssue where

import qualified Data.Text               as T (unpack)
import qualified Data.Vector             as Vector (fromList)

import qualified GitHub.Auth             as GitHub
import qualified GitHub.Data.Issues      as GitHub
import qualified GitHub.Endpoints.Issues as GitHub


main :: IO ()
main = do
  let auth = GitHub.BasicAuth "user" "password"
      newiss = (GitHub.newIssue "A new issue")
        { GitHub.newIssueBody = Just "Issue description text goes here"
        -- UnComment to add issue labels.
        -- , GitHub.newIssueLabels = Just $ Vector.fromList ["foo", "bar", "baz"]
        }
  possibleIssue <- GitHub.createIssue auth "thoughtbot" "paperclip" newiss
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatIssue
                    possibleIssue

formatIssue :: GitHub.Issue -> String
formatIssue issue =
  -- (T.unpack $ GitHub.simpleOwnerLogin $ GitHub.issueUser issue) ++
    " opened this issue " ++
    (show $ GitHub.issueCreatedAt issue) ++ "\n" ++
    (T.unpack $ GitHub.issueState issue) ++ " with " ++
    (show $ GitHub.issueComments issue) ++ " comments" ++ "\n\n" ++
    (T.unpack $ GitHub.issueTitle issue)
