{-# LANGUAGE CPP #-}

-- |
-- The issues API as described on <http://developer.github.com/v3/issues/>.

module GitHub.Endpoints.Issues (
    currentUserIssuesR,
    organizationIssuesR,
    issueR,
    issuesForRepoR,
    issuesForRepoPagedR,
    createIssueR,
    newIssue,
    editIssueR,
    editOfIssue,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | See <https://developer.github.com/v3/issues/#list-issues>.
currentUserIssuesR :: IssueMod -> FetchCount -> Request 'RA (Vector Issue)
currentUserIssuesR opts =
    pagedQuery ["user", "issues"] (issueModToQueryString opts)

-- | See <https://developer.github.com/v3/issues/#list-issues>.
organizationIssuesR :: Name Organization -> IssueMod -> FetchCount -> Request k (Vector Issue)
organizationIssuesR org opts =
    pagedQuery ["orgs", toPathPart org, "issues"] (issueModToQueryString opts)

-- | Query a single issue.
-- See <https://developer.github.com/v3/issues/#get-a-single-issue>
issueR :: Name Owner -> Name Repo -> IssueNumber -> Request k Issue
issueR user reqRepoName reqIssueNumber =
    query ["repos", toPathPart user, toPathPart reqRepoName, "issues", toPathPart reqIssueNumber] []

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
issuesForRepoR user reqRepoName opts =
    pagedQuery ["repos", toPathPart user, toPathPart reqRepoName, "issues"] qs
  where
    qs = issueRepoModToQueryString opts

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoPagedR :: Name Owner -> Name Repo -> IssueRepoMod -> PageParams -> Request k (Vector Issue, PageLinks)
issuesForRepoPagedR user reqRepoName opts =
    perPageQuery ["repos", toPathPart user, toPathPart reqRepoName, "issues"] qs
  where
    qs = issueRepoModToQueryString opts

-- Creating new issues.

newIssue :: Text -> NewIssue
newIssue title = NewIssue title Nothing mempty Nothing Nothing

-- | Create an issue.
-- See <https://developer.github.com/v3/issues/#create-an-issue>
createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request 'RW Issue
createIssueR user repo =
    command Post ["repos", toPathPart user, toPathPart repo, "issues"] . encode

-- Editing issues.

editOfIssue :: EditIssue
editOfIssue = EditIssue Nothing Nothing Nothing Nothing Nothing Nothing

-- | Edit an issue.
-- See <https://developer.github.com/v3/issues/#edit-an-issue>
editIssueR :: Name Owner -> Name Repo -> IssueNumber -> EditIssue -> Request 'RW Issue
editIssueR user repo iss =
    command Patch ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss] . encode
