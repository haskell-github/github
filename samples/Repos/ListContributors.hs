module ListContributors where

import qualified Github.Repos as Github
import Data.List

main = do
  possibleContributors <- Github.contributors "thoughtbot" "paperclip"
  case possibleContributors of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right contributors) ->
         putStrLn $ intercalate "\n" $ map formatContributor contributors

formatContributor contributor =
  (show $ Github.contributorContributions contributor) ++ "\t" ++
    (Github.contributorLogin contributor)
