module ShowMembers where

import qualified Github.Organizations.Members as Github
import Data.List (intercalate)
import Data.Default (def)

main = do
  possibleMembers <- Github.membersOf def "thoughtbot"
  case possibleMembers of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right members) ->
         putStrLn $ intercalate "\n" $ map Github.githubOwnerLogin members
