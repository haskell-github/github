{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The issues API as described on <http://developer.github.com/v3/issues/>.
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

import Github.Auth
import Github.Data
import Github.Request

import Control.DeepSeq   (NFData)
import Data.Aeson.Compat (encode)
import Data.Data
import Data.List         (intercalate)
import Data.Text         (Text)
#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import GHC.Generics (Generic)

import Data.Time.Clock  (UTCTime (..))
import Data.Time.Format (formatTime)


-- | A data structure for describing how to filter issues. This is used by
-- @issuesForRepo@.
data IssueLimitation =
      AnyMilestone -- ^ Issues appearing in any milestone. [default]
    | NoMilestone -- ^ Issues without a milestone.
    | MilestoneId Int -- ^ Only issues that are in the milestone with the given id.
    | Open -- ^ Only open issues. [default]
    | OnlyClosed -- ^ Only closed issues.
    | Unassigned -- ^ Issues to which no one has been assigned ownership.
    | AnyAssignment -- ^ All issues regardless of assignment. [default]
    | AssignedTo String -- ^ Only issues assigned to the user with the given login.
    | Mentions String -- ^ Issues which mention the given string, taken to be a user's login.
    | Labels [String] -- ^ A list of labels to filter by.
    | Ascending -- ^ Sort ascending.
    | Descending -- ^ Sort descending. [default]
    | Since UTCTime -- ^ Only issues created since the specified date and time.
    | PerPage Int -- ^ Download this many issues per query
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData IssueLimitation

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
    GithubGet ["repos", untagName user, untagName reqRepoName, "issues", show $ untagId reqIssueNumber] ""

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo' auth user reqRepoName issueLimitations =
    executeRequestMaybe auth $ issuesForRepoR user reqRepoName issueLimitations

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: Name GithubOwner -> Name Repo -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo = issuesForRepo' Nothing

-- | List issues for a repository.
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
issuesForRepoR :: Name GithubOwner -> Name Repo -> [IssueLimitation] -> GithubRequest k [Issue]
issuesForRepoR user reqRepoName issueLimitations =
    GithubGet ["repos", untagName user, untagName reqRepoName, "issues"] qs
  where
    qs = queryStringFromLimitations issueLimitations
    queryStringFromLimitations = intercalate "&" . map convert

    convert AnyMilestone     = "milestone=*"
    convert NoMilestone      = "milestone=none"
    convert (MilestoneId n)  = "milestone=" ++ show n
    convert Open             = "state=open"
    convert OnlyClosed       = "state=closed"
    convert Unassigned       = "assignee=none"
    convert AnyAssignment    = "assignee=*"
    convert (AssignedTo u)   = "assignee=" ++ u
    convert (Mentions u)     = "mentioned=" ++ u
    convert (Labels l)       = "labels=" ++ intercalate "," l
    convert Ascending        = "direction=asc"
    convert Descending       = "direction=desc"
    convert (PerPage n)      = "per_page=" ++ show n
    convert (Since t)        =
      "since=" ++ formatTime defaultTimeLocale "%FT%TZ" t

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
    GithubPost Post ["repos", untagName user, untagName repo, "issues"] . encode

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
    GithubPost Patch ["repos", untagName user, untagName repo, "issues", show $ untagId iss] . encode
