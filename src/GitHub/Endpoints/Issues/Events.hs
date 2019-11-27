-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github issue events API, which is described on
-- <http://developer.github.com/v3/issues/events/>
module GitHub.Endpoints.Issues.Events (
    eventsForIssueR,
    eventsForRepoR,
    eventR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List events for an issue.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-an-issue>
eventsForIssueR :: Name Owner -> Name Repo -> Id Issue -> FetchCount -> Request k (Vector IssueEvent)
eventsForIssueR user repo iid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iid, "events"] []

-- | List events for a repository.
-- See <https://developer.github.com/v3/issues/events/#list-events-for-a-repository>
eventsForRepoR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector IssueEvent)
eventsForRepoR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "issues", "events"] []

-- | Query a single event.
-- See <https://developer.github.com/v3/issues/events/#get-a-single-event>
eventR :: Name Owner -> Name Repo -> Id IssueEvent -> Request k IssueEvent
eventR user repo eid =
    query ["repos", toPathPart user, toPathPart repo, "issues", "events", toPathPart eid] []
