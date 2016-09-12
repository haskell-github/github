{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The issues API as described on <http://developer.github.com/v3/issues/>.
module GitHub.Endpoints.Issues (
    issue,
    issue',
    issueR,
    issuesForRepo,
    issuesForRepo',
    issuesForRepoR,
    IssueLimitation(..),
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

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

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
    Query ["repos", toPathPart user, toPathPart reqRepoName, "issues", toPathPart reqIssueNumber] []

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo' :: Maybe Auth -> Name Owner -> Name Repo -> [IssueLimitation] -> IO (Either Error (Vector Issue))
issuesForRepo' auth user reqRepoName issueLimitations =
    executeRequestMaybe auth $ issuesForRepoR user reqRepoName issueLimitations FetchAll

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: Name Owner -> Name Repo -> [IssueLimitation] -> IO (Either Error (Vector Issue))
issuesForRepo = issuesForRepo' Nothing

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoR :: Name Owner -> Name Repo -> [IssueLimitation] -> FetchCount -> Request k (Vector Issue)
issuesForRepoR user reqRepoName issueLimitations =
    PagedQuery ["repos", toPathPart user, toPathPart reqRepoName, "issues"] qs
  where
    qs = map convert issueLimitations

    convert AnyMilestone     = ("milestone", Just "*")
    convert NoMilestone      = ("milestone", Just "none")
    convert (MilestoneId n)  = ("milestone", Just . TE.encodeUtf8 . T.pack $ show n)
    convert Open             = ("state", Just "open")
    convert OnlyClosed       = ("state", Just "closed")
    convert Unassigned       = ("assignee", Just "none")
    convert AnyAssignment    = ("assignee", Just "")
    convert (AssignedTo u)   = ("assignee", Just . TE.encodeUtf8 . T.pack $ u)
    convert (Mentions u)     = ("mentioned", Just . TE.encodeUtf8 . T.pack $ u)
    convert (Labels l)       = ("labels", Just . TE.encodeUtf8 . T.pack $ intercalate "," l)
    convert Ascending        = ("direction", Just "asc")
    convert Descending       = ("direction", Just "desc")
    convert (PerPage n)      = ("per_page", Just . TE.encodeUtf8 . T.pack $ show n)
    convert (Since t)        = ("since", Just . TE.encodeUtf8 . T.pack $ formatISO8601 t)

-- Creating new issues.

newIssue :: Text -> NewIssue
newIssue title = NewIssue title Nothing Nothing Nothing Nothing


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
createIssueR :: Name Owner -> Name Repo -> NewIssue -> Request 'True Issue
createIssueR user repo =
    Command Post ["repos", toPathPart user, toPathPart repo, "issues"] . encode

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
editIssueR :: Name Owner -> Name Repo -> Id Issue -> EditIssue -> Request 'True Issue
editIssueR user repo iss =
    Command Patch ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss] . encode
