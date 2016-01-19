{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The issues API as described on <http://developer.github.com/v3/issues/>.
module Github.Issues (
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
    module Github.Data,
    ) where

import Github.Data
import Github.Request

import Data.Aeson.Compat (encode)
import Data.List         (intercalate)
import Data.Text         (Text)
import Data.Time.ISO8601 (formatISO8601)
import Data.Vector       (Vector)

import qualified Data.ByteString.Char8 as BS8

-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.'
--
-- > issue' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "462"
issue' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error Issue)
issue' auth user reqRepoName reqIssueNumber =
    executeRequestMaybe auth $ issueR user reqRepoName reqIssueNumber

-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.
--
-- > issue "thoughtbot" "paperclip" (Id "462")
issue :: Name GithubOwner -> Name Repo -> Id Issue -> IO (Either Error Issue)
issue = issue' Nothing

-- | Get a single issue.
-- See <https://developer.github.com/v3/issues/#get-a-single-issue>
issueR :: Name GithubOwner -> Name Repo -> Id Issue -> GithubRequest k Issue
issueR user reqRepoName reqIssueNumber =
    GithubGet ["repos", toPathPart user, toPathPart reqRepoName, "issues", toPathPart reqIssueNumber] []

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> [IssueLimitation] -> IO (Either Error (Vector Issue))
issuesForRepo' auth user reqRepoName issueLimitations =
    executeRequestMaybe auth $ issuesForRepoR user reqRepoName issueLimitations Nothing

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: Name GithubOwner -> Name Repo -> [IssueLimitation] -> IO (Either Error (Vector Issue))
issuesForRepo = issuesForRepo' Nothing

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoR :: Name GithubOwner -> Name Repo -> [IssueLimitation] -> Maybe Count -> GithubRequest k (Vector Issue)
issuesForRepoR user reqRepoName issueLimitations =
    GithubPagedGet ["repos", toPathPart user, toPathPart reqRepoName, "issues"] qs
  where
    qs = map convert issueLimitations

    convert AnyMilestone     = ("milestone", Just "*")
    convert NoMilestone      = ("milestone", Just "none")
    convert (MilestoneId n)  = ("milestone", Just . BS8.pack $ show n)
    convert Open             = ("state", Just "open")
    convert OnlyClosed       = ("state", Just "closed")
    convert Unassigned       = ("assignee", Just "none")
    convert AnyAssignment    = ("assignee", Just "")
    convert (AssignedTo u)   = ("assignee", Just $ BS8.pack u)
    convert (Mentions u)     = ("mentioned", Just $ BS8.pack u)
    convert (Labels l)       = ("labels", Just . BS8.pack $ intercalate "," l)
    convert Ascending        = ("direction", Just "asc")
    convert Descending       = ("direction", Just "desc")
    convert (PerPage n)      = ("per_page", Just . BS8.pack $ show n)
    convert (Since t)        = ("since", Just . BS8.pack $ formatISO8601 t)

-- Creating new issues.

newIssue :: Text -> NewIssue
newIssue title = NewIssue title Nothing Nothing Nothing Nothing


-- | Create a new issue.
--
-- > createIssue (GithubUser (user, password)) user repo
-- >  (newIssue "some_repo") {...}
createIssue :: GithubAuth -> Name GithubOwner -> Name Repo -> NewIssue
            -> IO (Either Error Issue)
createIssue auth user repo ni =
     executeRequest auth $ createIssueR user repo ni

-- | Create an issue.
-- See <https://developer.github.com/v3/issues/#create-an-issue>
createIssueR :: Name GithubOwner -> Name Repo -> NewIssue -> GithubRequest 'True Issue
createIssueR user repo =
    GithubCommand Post ["repos", toPathPart user, toPathPart repo, "issues"] . encode

-- Editing issues.

editOfIssue :: EditIssue
editOfIssue = EditIssue Nothing Nothing Nothing Nothing Nothing Nothing

-- | Edit an issue.
--
-- > editIssue (GithubUser (user, password)) user repo issue
-- >  editOfIssue {...}
editIssue :: GithubAuth -> Name GithubOwner -> Name Repo -> Id Issue -> EditIssue
            -> IO (Either Error Issue)
editIssue auth user repo iss edit =
     executeRequest auth $ editIssueR user repo iss edit

-- | Edit an issue.
-- See <https://developer.github.com/v3/issues/#edit-an-issue>
editIssueR :: Name GithubOwner -> Name Repo -> Id Issue -> EditIssue -> GithubRequest 'True Issue
editIssueR user repo iss =
    GithubCommand Patch ["repos", toPathPart user, toPathPart repo, "issues", toPathPart iss] . encode
