{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.String                       (fromString)
import qualified Data.Text               as Text   (unpack)
import qualified Data.Vector             as Vector (fromList)
import qualified GitHub.Auth             as GitHub
import qualified GitHub.Data.Issues      as GitHub
import qualified GitHub.Endpoints.Issues as GitHub
import qualified GitHub.Request          as GitHub

import           System.Environment                (lookupEnv)
import qualified System.Exit             as Exit   (die)

self :: String
self = "github-create-issue"

main :: IO ()
main = do
  token <- lookupEnv "GITHUB_TOKEN" >>= \case
    Nothing    -> die "variable GITHUB_TOKEN not set"
    Just token -> return $ fromString token

  let auth    = GitHub.OAuth token
      newiss  = (GitHub.newIssue "A new issue")
        { GitHub.newIssueBody   = Just "Issue description text goes here"
        , GitHub.newIssueLabels = Just $ Vector.fromList ["foo", "bar", "baz"]
        }
      request = GitHub.createIssueR "haskell-github" "playground" newiss

  GitHub.github auth request >>= \case
    Left  err   -> die $ show err
    Right issue -> putStrLn $ formatIssue issue

die :: String -> IO a
die msg = Exit.die $ concat [ self, ": Error: ", msg ]

formatIssue :: GitHub.Issue -> String
formatIssue issue = concat
  [ formatUser issue
  , " opened this issue "
  , show $ GitHub.issueCreatedAt issue
  , "\n"
  , show $ GitHub.issueState issue
  , " with "
  , show $ GitHub.issueComments issue
  , " comments\n\n"
  , Text.unpack $ GitHub.issueTitle issue
  ]

formatUser :: GitHub.Issue -> String
formatUser issue =
  Text.unpack . GitHub.untagName . GitHub.simpleUserLogin $ GitHub.issueUser issue
