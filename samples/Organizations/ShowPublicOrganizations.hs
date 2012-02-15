module ShowPublicOrganizations where

import qualified Github.Organizations as Github
import Data.List (intercalate)

main = do
  possibleOrganizations <- Github.publicOrganizationsFor "mike-burns"
  case possibleOrganizations of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right organizations) ->
         putStrLn $ intercalate "\n" $ map formatOrganization organizations

formatOrganization = Github.simpleOrganizationLogin
