module ShowPublicOrganizations where

import qualified Github.Organizations as Github
import Data.List (intercalate)
import Data.Default (def)

main = do
  possibleOrganizations <- Github.publicOrganizationsFor def "mike-burns"
  case possibleOrganizations of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right organizations) ->
         putStrLn $ intercalate "\n" $ map formatOrganization organizations

formatOrganization = Github.simpleOrganizationLogin
