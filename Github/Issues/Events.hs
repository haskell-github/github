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
eventsForIssue user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber, "events"]

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: String -> String -> IO (Either Error [Event])
eventsForRepo user repoName =
  githubGet ["repos", user, repoName, "issues", "events"]

-- | Details on a specific event, by the event's ID.
--
-- > event "thoughtbot" "paperclip" 5335772
event :: String -> String -> Int -> IO (Either Error Event)
event user repoName eventId =
  githubGet ["repos", user, repoName, "issues", "events", show eventId]
