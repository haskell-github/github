{-# LANGUAGE CPP, OverloadedStrings #-}
-- | The issues API as described on <http://developer.github.com/v3/issues/>.
module Github.Issues (
 issue
,issue'
,issuesForRepo
,issuesForRepo'
,IssueLimitation(..)
,createIssue
,newIssue
,editIssue
,editOfIssue
,module Github.Data
) where

import Github.Data
import Github.Private
import Data.List (intercalate)
#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import Data.Time.Format (formatTime)
import Data.Time.Clock (UTCTime(..))

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


-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.'
--
-- > issue' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "462"
issue' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error Issue)
issue' auth user reqRepoName reqIssueNumber =
  githubGet' auth ["repos", user, reqRepoName, "issues", show reqIssueNumber]

-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.
--
-- > issue "thoughtbot" "paperclip" "462"
issue :: String -> String -> Int -> IO (Either Error Issue)
issue = issue' Nothing

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo' :: Maybe GithubAuth -> String -> String -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo' auth user reqRepoName issueLimitations =
  githubGetWithQueryString' 
    auth
    ["repos", user, reqRepoName, "issues"]
    (queryStringFromLimitations issueLimitations)
  where
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

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: String -> String -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo = issuesForRepo' Nothing


-- Creating new issues.

newIssue :: String -> NewIssue
newIssue title = NewIssue title Nothing Nothing Nothing Nothing


-- |
-- Create a new issue.
--
-- > createIssue (GithubUser (user, password)) user repo
-- >  (newIssue "some_repo") {...}
createIssue :: GithubAuth -> String -> String -> NewIssue
            -> IO (Either Error Issue)
createIssue auth user repo = githubPost auth ["repos", user, repo, "issues"]


-- Editing issues.

editOfIssue :: EditIssue
editOfIssue = EditIssue Nothing Nothing Nothing Nothing Nothing Nothing


-- |
-- Edit an issue.
--
-- > editIssue (GithubUser (user, password)) user repo issue
-- >  editOfIssue {...}
editIssue :: GithubAuth -> String -> String -> Int -> EditIssue
            -> IO (Either Error Issue)
editIssue auth user repo iss =
  githubPatch auth ["repos", user, repo, "issues", show iss]
