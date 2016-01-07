-- | The Github issue events API, which is described on
-- <http://developer.github.com/v3/issues/events/>
module Github.Issues.Events (
    eventsForIssue,
    eventsForIssue',
    eventsForIssueR,
    eventsForRepo,
    eventsForRepo',
    eventsForRepoR,
    event,
    event',
    eventR,
    module Github.Data,
    ) where

import Github.Auth
import Github.Data
import Github.Request

-- | All events that have happened on an issue.
--
-- > eventsForIssue "thoughtbot" "paperclip" 49
eventsForIssue :: Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error [Event])
eventsForIssue = eventsForIssue' Nothing

-- | All events that have happened on an issue, using authentication.
--
-- > eventsForIssue' (GithubUser (user, password)) "thoughtbot" "paperclip" 49
eventsForIssue' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error [Event])
eventsForIssue' auth user repo iid =
    executeRequestMaybe auth $ eventsForIssueR user repo iid

-- | List events for an issue.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-an-issue>
eventsForIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> GithubRequest k [Event]
eventsForIssueR user repo iid =
    GithubGet ["repos", untagName user, untagName repo, "issues", show $ untagId iid, "events"] []

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: Name GithubOwner -> Name Repo -> IO (Either Error [Event])
eventsForRepo = eventsForRepo' Nothing

-- | All the events for all issues in a repo, using authentication.
--
-- > eventsForRepo' (GithubUser (user, password)) "thoughtbot" "paperclip"
eventsForRepo' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error [Event])
eventsForRepo' auth user repo =
    executeRequestMaybe auth $ eventsForRepoR user repo

-- | List events for a repository.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-a-repository>
eventsForRepoR :: Name GithubOwner -> Name Repo -> GithubRequest k [Event]
eventsForRepoR user repo =
    GithubGet ["repos", untagName user, untagName repo, "issues", "events"] []

-- | Details on a specific event, by the event's ID.
--
-- > event "thoughtbot" "paperclip" 5335772
event :: Name GithubOwner -> Name Repo -> Id Event -> IO (Either Error Event)
event = event' Nothing

-- | Details on a specific event, by the event's ID, using authentication.
--
-- > event' (GithubUser (user, password)) "thoughtbot" "paperclip" 5335772
event' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Event -> IO (Either Error Event)
event' auth user repo eid =
    executeRequestMaybe auth $ eventR user repo eid

-- | Get a single event.
-- See <https://developer.github.com/v3/issues/events/#get-a-single-event>
eventR :: Name GithubOwner -> Name Repo -> Id Event -> GithubRequest k Event
eventR user repo eid =
    GithubGet ["repos", untagName user, untagName repo, "issues", "events", show eid] []
