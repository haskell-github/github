module ListFollowers where

import qualified Github.Users.Followers as Github
import Data.List (intercalate)

main = do
  possibleUsers <- Github.usersFollowing "mike-burns"
  putStrLn $ either (("Error: "++) . show)
                    (intercalate "\n" . map formatUser)
                    possibleUsers

formatUser = Github.githubOwnerLogin
