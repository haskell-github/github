-- | The Github issue events API, which is described on
-- <http://developer.github.com/v3/issues/events/>
module Github.Issues.Events (
 eventsForIssue
,eventsForIssue'
,eventsForRepo
,eventsForRepo'
,event
,event'
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

-- | All events that have happened on an issue, using authentication.
--
-- > eventsForIssue' (GithubUser (user, password)) "thoughtbot" "paperclip" 49
eventsForIssue' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [Event])
eventsForIssue' auth user reqRepoName reqIssueNumber =
  githubGet' auth ["repos", user, reqRepoName, "issues", show reqIssueNumber, "events"]

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: String -> String -> IO (Either Error [Event])
eventsForRepo user reqRepoName =
  githubGet ["repos", user, reqRepoName, "issues", "events"]

-- | All the events for all issues in a repo, using authentication.
--
-- > eventsForRepo' (GithubUser (user, password)) "thoughtbot" "paperclip"
eventsForRepo' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Event])
eventsForRepo' auth user reqRepoName =
  githubGet' auth ["repos", user, reqRepoName, "issues", "events"]

-- | Details on a specific event, by the event's ID.
--
-- > event "thoughtbot" "paperclip" 5335772
event :: String -> String -> Int -> IO (Either Error Event)
event user reqRepoName reqEventId =
  githubGet ["repos", user, reqRepoName, "issues", "events", show reqEventId]

-- | Details on a specific event, by the event's ID, using authentication.
--
-- > event' (GithubUser (user, password)) "thoughtbot" "paperclip" 5335772
event' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error Event)
event' auth user reqRepoName reqEventId =
  githubGet' auth ["repos", user, reqRepoName, "issues", "events", show reqEventId]
