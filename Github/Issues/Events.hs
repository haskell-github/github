module Github.Issues.Events (
 eventsForIssue
,module Github.Data
) where

import Github.Data
import Github.Private

eventsForIssue :: String -> String -> Int -> IO (Either Error [Event])
eventsForIssue user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber, "events"]
