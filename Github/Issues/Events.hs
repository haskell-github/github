-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github issue events API, which is described on
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

import Data.Vector (Vector)

import Github.Data
import Github.Request

-- | All events that have happened on an issue.
--
-- > eventsForIssue "thoughtbot" "paperclip" 49
eventsForIssue :: Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector Event))
eventsForIssue = eventsForIssue' Nothing

-- | All events that have happened on an issue, using authentication.
--
-- > eventsForIssue' (GithubUser (user, password)) "thoughtbot" "paperclip" 49
eventsForIssue' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error (Vector Event))
eventsForIssue' auth user repo iid =
    executeRequestMaybe auth $ eventsForIssueR user repo iid Nothing

-- | List events for an issue.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-an-issue>
eventsForIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> Maybe Count -> GithubRequest k (Vector Event)
eventsForIssueR user repo iid =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "events"] []

-- | All the events for all issues in a repo.
--
-- > eventsForRepo "thoughtbot" "paperclip"
eventsForRepo :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Event))
eventsForRepo = eventsForRepo' Nothing

-- | All the events for all issues in a repo, using authentication.
--
-- > eventsForRepo' (GithubUser (user, password)) "thoughtbot" "paperclip"
eventsForRepo' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Event))
eventsForRepo' auth user repo =
    executeRequestMaybe auth $ eventsForRepoR user repo Nothing

-- | List events for a repository.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-a-repository>
eventsForRepoR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector Event)
eventsForRepoR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "issues", "events"] []

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
    GithubGet ["repos", toPathPart user, toPathPart repo, "issues", "events", show eid] []
