module ListContributorsWithAnonymous where

import qualified Github.Repos as Github
import Data.List

main = do
  possibleContributors <- Github.contributorsWithAnonymous "thoughtbot" "paperclip"
  case possibleContributors of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right contributors) ->
         putStrLn $ intercalate "\n" $ map formatContributor contributors

formatContributor (Github.KnownContributor contributions _ login _ _ _) =
  (show $ contributions) ++ "\t" ++ login
formatContributor (Github.AnonymousContributor contributions name) =
  (show $ contributions) ++ "\t" ++ name
