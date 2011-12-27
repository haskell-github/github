module ShowUser where

import qualified Github.Users as Github
import Data.Maybe (fromMaybe)

main = do
  possibleUser <- Github.userInfoFor "mike-burns"
  putStrLn $ either (("Error: "++) . show) formatUser possibleUser

formatUser user =
  (formatName userName login) ++ "\t" ++ (fromMaybe "" company) ++ "\t" ++
    (fromMaybe "" location) ++ "\n" ++
    (fromMaybe "" blog) ++ "\t" ++ "<" ++ email ++ ">" ++ "\n" ++
    htmlUrl ++ "\t" ++ (formatDate createdAt) ++ "\n" ++
    "hireable: " ++ (formatHireable isHireable) ++ "\n\n" ++
    (fromMaybe "" bio)
  where
    userName = Github.detailedUserName user
    login = Github.detailedUserLogin user
    company = Github.detailedUserCompany user
    location = Github.detailedUserLocation user
    blog = Github.detailedUserBlog user
    email = Github.detailedUserEmail user 
    htmlUrl = Github.detailedUserHtmlUrl user
    createdAt = Github.detailedUserCreatedAt user
    isHireable = Github.detailedUserHireable user
    bio = Github.detailedUserBio user

formatName Nothing login = login
formatName (Just name) login = name ++ "(" ++ login ++ ")"

formatHireable True = "yes"
formatHireable False = "no"

formatDate = show . Github.fromGithubDate
