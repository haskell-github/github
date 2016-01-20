{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Github.Auth as Github
import qualified Github.Issues as Github
import Report

-- The example requires wl-pprint module "The Wadler/Leijen Pretty Printer"
import Text.PrettyPrint.ANSI.Leijen

auth ::  Maybe Github.Auth
auth = Just $ Github.EnterpriseOAuth
                    "https://github.example.com/api"
                    "1a79a4d60de6718e8e5b326e338ae533"

mkIssue :: ReportedIssue -> Doc
mkIssue (Issue n t h) = hsep [
        fill 5  (text ("#" ++ (show n))),
        fill 50 (text t),
        fill 5 (text (show h))]

vissues :: ([Doc], [Doc], [Doc]) -> Doc
vissues (x, y, z) = hsep [(vcat x), align (vcat y), align (vcat z)] 

mkDoc :: Report -> Doc
mkDoc (Report issues total) = vsep [
                text "Report for the milestone",
                (vsep . map mkIssue) issues, 
                text ("Total hours : " ++ (show total) ++" hours")
        ]

mkFullDoc ::  [Github.Issue] -> Doc
mkFullDoc = mkDoc . prepareReport

-- The public repo is used as private are quite sensitive for this report
-- 
-- The main idea is to use labels like 1h, 2h etc for man-hour estimation of issues
-- on private repos for development "on hire"
--
-- This tool is used to generate report on work done for the customer
--
main ::  IO ()
main = do
  let limitations = [Github.OnlyClosed, Github.MilestoneId 4]
  possibleIssues <- Github.issuesForRepo' auth  "paulrzcz" "hquantlib" limitations
  case possibleIssues of
       (Left err) -> putStrLn $ "Error: " ++ show err
       (Right issues) -> putDoc $ mkFullDoc issues 
