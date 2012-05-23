{-# LANGUAGE OverloadedStrings #-}
module Report (
    ReportedIssue (..),
    Report (..),
    prepareReport,
    convertLabels
    ) where

import Text.Regex.Posix
import qualified Github.Issues as Github

data ReportedIssue = Issue {
        riNumber :: Int,
        riTitle  :: String,
        riHour   :: Double
    } deriving (Show)

data Report = Report {
        rIssues  :: [ReportedIssue],
        rTotal   :: Double
    } deriving (Show)

convertIssue :: Github.Issue -> ReportedIssue
convertIssue issue = Issue {
        riNumber = Github.issueNumber issue,
        riTitle  = Github.issueTitle  issue,
        riHour   = convertLabels issue
    }

convertLabels ::  Github.Issue -> Double
convertLabels = sumUp . toNames . Github.issueLabels

prepareReport :: [Github.Issue] -> Report
prepareReport issues = Report {
        rIssues = reportedIssues,
        rTotal  = foldl summator 0 reportedIssues
    } where reportedIssues = map convertIssue issues
            summator z x   = z + (riHour x)

-- Helper functions to construct a sum of hour labels

sumUp ::  [Maybe Double] -> Double
sumUp = foldl s 0.0
        where   s z Nothing = z
                s z (Just x) = z+x

toNames ::  [Github.IssueLabel] -> [Maybe Double]
toNames = map (toValue . Github.labelName) 

isValue :: String -> Bool
isValue label = (label =~ ("^[0-9]h" :: String)) :: Bool

convert ::  Read a => [Char] -> a
convert label = read $ take len label
        where   len = (length label) - 1

toValue ::  Read a => String -> Maybe a
toValue label
   | isValue label      = Just (convert label)
   | otherwise          = Nothing

