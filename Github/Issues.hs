-- | The issues API as described on <http://developer.github.com/v3/issues/>.
module Github.Issues (
 issue
,issue'
,issuesForRepo
,issuesForRepo'
,IssueLimitation(..)
,module Github.Data
) where

import Github.Data
import Github.Private
import Data.List (intercalate)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
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


-- | Details on a specific issue, given the repo owner and name, and the issue
-- number.'
--
-- > issue' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "462"
issue' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error Issue)
issue' auth user repoName issueNumber =
  githubGet' auth ["repos", user, repoName, "issues", show issueNumber]

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
issuesForRepo' auth user repoName issueLimitations =
  githubGetWithQueryString' 
    auth
    ["repos", user, repoName, "issues"]
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
    convert (Since t)        =
      "since=" ++ formatTime defaultTimeLocale "%FT%TZ" t

-- | All issues for a repo (given the repo owner and name), with optional
-- restrictions as described in the @IssueLimitation@ data type.
--
-- > issuesForRepo "thoughtbot" "paperclip" [NoMilestone, OnlyClosed, Mentions "jyurek", Ascending]
issuesForRepo :: String -> String -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo = issuesForRepo' Nothing
