{-# LANGUAGE OverloadedStrings #-}
module ShowComment where

import qualified GitHub.Endpoints.PullRequests.Comments as GitHub
import GitHub.Data.Id (Id(Id))
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Time.Format

main :: IO ()
main = do
  possiblePullRequestComment <- GitHub.pullRequestComment "thoughtbot" "factory_girl" (Id 301819)
  case possiblePullRequestComment of
       (Left err)     -> putStrLn $ "Error: " <> show err
       (Right comment) -> putStrLn . unpack $ formatComment comment

formatComment :: GitHub.Comment -> Text
formatComment comment =
    "Author: " <> formatAuthor (GitHub.commentUser comment) <>
    "\nUpdated: " <> pack (formatTime' (GitHub.commentUpdatedAt comment)) <>
    (maybe "" (\u -> "\nURL: " <> GitHub.getUrl u) $ GitHub.commentHtmlUrl comment) <>
    "\n\n" <> GitHub.commentBody comment

formatAuthor :: GitHub.SimpleUser -> Text
formatAuthor user =
  GitHub.untagName (GitHub.simpleUserLogin user) <> " (" <> GitHub.getUrl (GitHub.simpleUserUrl user) <> ")"

formatTime' :: (FormatTime t) => t -> String
formatTime' = formatTime defaultTimeLocale "%T, %F (%Z)"
