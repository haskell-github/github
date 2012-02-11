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
-- > eventsForIssue def "thoughtbot" "paperclip" 49
eventsForIssue :: GithubConfig -> String -> String -> Int -> IO (Either Error [Event])
eventsForIssue c user repoName issueNumber =
  githubGet c ["repos", user, repoName, "issues", show issueNumber, "events"]

-- | All the events for all issues in a repo.
--
-- > eventsForRepo def "thoughtbot" "paperclip"
eventsForRepo :: GithubConfig -> String -> String -> IO (Either Error [Event])
eventsForRepo c user repoName =
  githubGet c ["repos", user, repoName, "issues", "events"]

-- | Details on a specific event, by the event's ID.
--
-- > event def "thoughtbot" "paperclip" 5335772
event :: GithubConfig -> String -> String -> Int -> IO (Either Error Event)
event c user repoName eventId =
  githubGet c ["repos", user, repoName, "issues", "events", show eventId]
