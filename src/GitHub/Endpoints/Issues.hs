{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The issues API as described on <http://developer.github.com/v3/issues/>.
module GitHub.Endpoints.Issues (
    currentUserIssuesR,
    organizationIssuesR,
    issue,
    issue',
    issueR,
    issuesForRepo,
    issuesForRepo',
    issuesForRepoR,
    createIssue,
    createIssueR,
    newIssue,
    editIssue,
    editIssueR,
    editOfIssue,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | See <https://developer.github.com/v3/issues/#list-issues>.
currentUserIssuesR :: IssueMod -> FetchCount -> Request 'RA (Vector Issue)
currentUserIssuesR opts =
    pagedQuery ["user", "issues"] (issueModToQueryString opts)

-- | See <https://developer.github.com/v3/issues/#list-issues>.
organizationIssuesR :: Name Organization -> IssueMod -> FetchCount -> Request k (Vector Issue)
organizationIssuesR org opts =
    pagedQuery ["orgs", toPathPart org, "issues"] (issueModToQueryString opts)

-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.'
--
-- > issue' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "462"
issue' :: Maybe Auth -> Name Owner -> Name Repo -> Id Issue -> IO (Either Error Issue)
issue' auth user reqRepoName reqIssueNumber =
    executeRequestMaybe auth $ issueR user reqRepoName reqIssueNumber

-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.
--
-- > issue "thoughtbot" "paperclip" (Id "462")
issue :: Name Owner -> Name Repo -> Id Issue -> IO (Either Error Issue)
issue = issue' Nothing

-- | Query a single issue.
-- See <https://developer.github.com/v3/issues/#get-a-single-issue>
issueR :: Name Owner -> Name Repo -> Id Issue -> Request k Issue
issueR user reqRepoName reqIssueNumber =
    query ["repos", toPathPart user, toPathPart reqRepoName, "issues", toPathPart reqIssueNumber] []

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the 'IssueRepoMod' data type.
--
-- > issuesForRepo' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo' :: Maybe Auth -> Name Owner -> Name Repo -> IssueRepoMod -> IO (Either Error (Vector Issue))
issuesForRepo' auth user reqRepoName opts =
    executeRequestMaybe auth $ issuesForRepoR user reqRepoName opts FetchAll

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the 'IssueRepoMod' data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: Name Owner -> Name Repo -> IssueRepoMod -> IO (Either Error (Vector Issue))
issuesForRepo = issuesForRepo' Nothing

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoR :: Name Owner -> Name Repo -> IssueRepoMod -> FetchCount -> Request k (Vector Issue)
issuesForRepoR user reqRepoName opts =
    pagedQuery ["repos", toPathPart user, toPathPart reqRepoName, "issues"] qs
  where
    qs = issueRepoModToQueryString opts

-- Creating new issues.

newIssue :: Text -> NewIssue
newIssue title = NewIssue title Nothing mempty Nothing Nothing


-- | Create a new issue.
--
-- > createIssue (User (user, password)) user repo
-- >  (newIssue "some_repo") {...}
createIssue :: Auth -> Name Owner -> Name Repo -> NewIssue
            -> IO (Either Error Issue)
createIssue auth user repo ni =
     executeRequest auth $ createIssueR user repo ni

-- | Create an issue.
-- See <https://developer.github.com/v3/issues/#create-an-issue>
createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request 'RW Issue
createIssueR user repo =
    command Post ["repos", toPathPart user, toPathPart repo, "issues"] . encode

-- Editing issues.

editOfIssue :: EditIssue
editOfIssue = EditIssue Nothing Nothing Nothing Nothing Nothing Nothing

-- | Edit an issue.
--
-- > editIssue (User (user, password)) user repo issue
-- >  editOfIssue {...}
editIssue :: Auth -> Name Owner -> Name Repo -> Id Issue -> EditIssue
            -> IO (Either Error Issue)
editIssue auth user repo iss edit =
     executeRequest auth $ editIssueR user repo iss edit

-- | Edit an issue.
-- See <https://developer.github.com/v3/issues/#edit-an-issue>
editIssueR :: Name Owner -> Name Repo -> Id Issue -> EditIssue -> Request 'RW Issue
editIssueR user repo iss =
    command Patch ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss] . encode
