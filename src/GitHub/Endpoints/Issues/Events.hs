-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github issue events API, which is described on
-- <http://developer.github.com/v3/issues/events/>
module GitHub.Endpoints.Issues.Events (
    eventsForIssue,
    eventsForIssue',
    eventsForIssueR,
    eventsForRepo,
    eventsForRepo',
    eventsForRepoR,
    event,
    event',
    eventR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Request
import GitHub.Internal.Prelude

-- | All events that have happened on an issue.
--
-- > eventsForIssue "thoughtbot" "paperclip" 49
eventsForIssue :: Name Owner -> Name Repo -> Id Issue -> IO (Either Error (Vector Event))
eventsForIssue = eventsForIssue' Nothing

-- | All events that have happened on an issue, using authentication.
--
-- > eventsForIssue' (User (user, password)) "thoughtbot" "paperclip" 49
eventsForIssue' :: Maybe Auth -> Name Owner -> Name Repo -> Id Issue -> IO (Either Error (Vector Event))
eventsForIssue' auth user repo iid =
    executeRequestMaybe auth $ eventsForIssueR user repo iid FetchAll

-- | List events for an issue.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-an-issue>
eventsForIssueR :: Name Owner -> Name Repo -> Id Issue -> FetchCount -> Request k (Vector Event)
eventsForIssueR user repo iid =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "events"] []

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: Name Owner -> Name Repo -> IO (Either Error (Vector Event))
eventsForRepo = eventsForRepo' Nothing

-- | All the events for all issues in a repo, using authentication.
--
-- > eventsForRepo' (User (user, password)) "thoughtbot" "paperclip"
eventsForRepo' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Event))
eventsForRepo' auth user repo =
    executeRequestMaybe auth $ eventsForRepoR user repo FetchAll

-- | List events for a repository.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-a-repository>
eventsForRepoR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Event)
eventsForRepoR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "issues", "events"] []

-- | Details on a specific event, by the event's ID.
--
-- > event "thoughtbot" "paperclip" 5335772
event :: Name Owner -> Name Repo -> Id Event -> IO (Either Error Event)
event = event' Nothing

-- | Details on a specific event, by the event's ID, using authentication.
--
-- > event' (User (user, password)) "thoughtbot" "paperclip" 5335772
event' :: Maybe Auth -> Name Owner -> Name Repo -> Id Event -> IO (Either Error Event)
event' auth user repo eid =
    executeRequestMaybe auth $ eventR user repo eid

-- | Query a single event.
-- See <https://developer.github.com/v3/issues/events/#get-a-single-event>
eventR :: Name Owner -> Name Repo -> Id Event -> Request k Event
eventR user repo eid =
    Query ["repos", toPathPart user, toPathPart repo, "issues", "events", show eid] []
