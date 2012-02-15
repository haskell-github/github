module ShowPublicOrganization where

import qualified Github.Organizations as Github

main = do
  possibleOrganization <- Github.publicOrganization "thoughtbot"
  case possibleOrganization of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right organization) ->
         putStrLn $ formatOrganization organization

formatOrganization organization =
  (maybe "" (\s -> s ++ "\n") (Github.organizationName organization)) ++
    (Github.organizationHtmlUrl organization) ++
    (maybe "" (\s -> "\n" ++ s) (Github.organizationBlog organization))
