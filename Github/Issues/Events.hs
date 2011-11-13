module Github.Issues.Events (
 eventsForIssue
,eventsForRepo
,event
,module Github.Data
) where

import Github.Data
import Github.Private

eventsForIssue :: String -> String -> Int -> IO (Either Error [Event])
eventsForIssue user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber, "events"]

eventsForRepo :: String -> String -> IO (Either Error [Event])
eventsForRepo user repoName =
  githubGet ["repos", user, repoName, "issues", "events"]

event :: String -> String -> Int -> IO (Either Error Event)
event user repoName eventId =
  githubGet ["repos", user, repoName, "issues", "events", show eventId]
