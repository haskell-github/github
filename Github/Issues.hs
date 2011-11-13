module Github.Issues (
 issue
,issuesForRepo
,IssueLimitation(..)
,module Github.Data
) where

import Github.Data
import Github.Private
import Data.List (intercalate)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime(..))

data IssueLimitation =
      AnyMilestone
    | NoMilestone
    | MilestoneId Int
    | Open
    | OnlyClosed
    | Unassigned
    | AnyAssignment
    | AssignedTo String
    | Mentions String
    | Labels [String]
    | Ascending
    | Descending
    | Since UTCTime

issue :: String -> String -> Int -> IO (Either Error Issue)
issue user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber]

issuesForRepo :: String -> String -> [IssueLimitation] -> IO (Either Error [Issue])
issuesForRepo user repoName issueLimitations =
  githubGetWithQueryString
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
