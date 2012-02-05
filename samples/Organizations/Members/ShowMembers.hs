module ShowMembers where

import qualified Github.Organizations.Members as Github
import Data.List (intercalate)

main = do
  possibleMembers <- Github.membersOf "thoughtbot"
  case possibleMembers of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right members) ->
         putStrLn $ intercalate "\n" $ map Github.githubOwnerLogin members
