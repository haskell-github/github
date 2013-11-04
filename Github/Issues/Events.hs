-- | The Github issue events API, which is described on
-- <http://developer.github.com/v3/issues/events/>
module Github.Issues.Events (
 eventsForIssue
,eventsForRepo
,event
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All events that have happened on an issue.
--
-- > eventsForIssue "thoughtbot" "paperclip" 49
eventsForIssue :: String -> String -> Int -> IO (Either Error [Event])
eventsForIssue user reqRepoName reqIssueNumber =
  githubGet ["repos", user, reqRepoName, "issues", show reqIssueNumber, "events"]

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: String -> String -> IO (Either Error [Event])
eventsForRepo user reqRepoName =
  githubGet ["repos", user, reqRepoName, "issues", "events"]

-- | Details on a specific event, by the event's ID.
--
-- > event "thoughtbot" "paperclip" 5335772
event :: String -> String -> Int -> IO (Either Error Event)
event user reqRepoName reqEventId =
  githubGet ["repos", user, reqRepoName, "issues", "events", show reqEventId]
