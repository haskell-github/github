module ShowUser where

import qualified Github.Users as Github
import Data.Maybe (fromMaybe)

main = do
  possibleUser <- Github.userInfoFor "mike-burns"
  putStrLn $ either (("Error: "++) . show) formatUser possibleUser

formatUser user@(Github.DetailedOrganization {}) =
  "Organization: " ++ (formatName userName login) ++ "\t" ++
    (fromMaybe "" company) ++ "\t" ++
    (fromMaybe "" location) ++ "\n" ++
    (fromMaybe "" blog) ++ "\t" ++ "\n" ++
    htmlUrl ++ "\t" ++ (formatDate createdAt) ++ "\n\n" ++
    (fromMaybe "" bio)
  where
    userName = Github.detailedOwnerName user
    login = Github.detailedOwnerLogin user
    company = Github.detailedOwnerCompany user
    location = Github.detailedOwnerLocation user
    blog = Github.detailedOwnerBlog user
    htmlUrl = Github.detailedOwnerHtmlUrl user
    createdAt = Github.detailedOwnerCreatedAt user
    bio = Github.detailedOwnerBio user

formatUser user@(Github.DetailedUser {}) =
  (formatName userName login) ++ "\t" ++ (fromMaybe "" company) ++ "\t" ++
    (fromMaybe "" location) ++ "\n" ++
    (fromMaybe "" blog) ++ "\t" ++ "<" ++ email ++ ">" ++ "\n" ++
    htmlUrl ++ "\t" ++ (formatDate createdAt) ++ "\n" ++
    "hireable: " ++ (formatHireable isHireable) ++ "\n\n" ++
    (fromMaybe "" bio)
  where
    userName = Github.detailedOwnerName user
    login = Github.detailedOwnerLogin user
    company = Github.detailedOwnerCompany user
    location = Github.detailedOwnerLocation user
    blog = Github.detailedOwnerBlog user
    email = Github.detailedOwnerEmail user 
    htmlUrl = Github.detailedOwnerHtmlUrl user
    createdAt = Github.detailedOwnerCreatedAt user
    isHireable = Github.detailedOwnerHireable user
    bio = Github.detailedOwnerBio user

formatName Nothing login = login
formatName (Just name) login = name ++ "(" ++ login ++ ")"

formatHireable True = "yes"
formatHireable False = "no"

formatDate = show . Github.fromGithubDate
